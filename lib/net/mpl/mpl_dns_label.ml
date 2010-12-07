(*
 * Copyright (c) 2010 Anil Madhavapeddy <anil@recoil.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

open OS.Istring.View

exception Bad_dns_label
type t = int * string list (* size * (bits list) *)

let (unmarshal_labels:((int, string list) Hashtbl.t)) = Hashtbl.create 1
let (marshal_labels:(string list, int option) Hashtbl.t) = Hashtbl.create 1

let marshal_base = ref 0
let unmarshal_base = ref 0

let init_unmarshal env =
  Hashtbl.clear unmarshal_labels; 
  unmarshal_base := env.off

let init_marshal env = 
  Hashtbl.clear marshal_labels;
  marshal_base := env.off

let dump () =
  Hashtbl.iter (fun k v ->
    Printf.printf "%s=%s " (String.concat "." k)
      (match v with
       | None -> "X" 
       | Some off -> string_of_int off
      )
  ) marshal_labels;
  print_newline ()

let of_string_list ?(comp=false) s : t =
  (* crap, we need to guess the length before we marshal it! *)
  let rec fn am = function
    | bit::r as x ->
      let found = Hashtbl.mem marshal_labels x in
      if comp && found then
        am + 2
      else begin
        (* insert embryonic marker *)
        if (not found) then Hashtbl.add marshal_labels x None;
        fn (String.length bit + 1 + am) r; (* for length of bit *)
      end
   | [] -> am + 1 in
  let sz = fn 0 s in
  sz,s

let to_string_list (s:t) =
  snd s

let marshal ?(comp=false) env ((psz,t):t) =
  let abspos env = env.off - !marshal_base + env.pos in
  let start_pos = env.pos in
  let insert_string env bit x =
    Hashtbl.add marshal_labels x (Some (abspos env));
    append_byte env (String.length bit);
    append_string env bit;
  in
  (* for each bit, figure out if it can be pointer instead of a label *)
  let rec fn = function
  | bit::r as x ->
      if comp then begin
        try
          match Hashtbl.find marshal_labels x with
            | None -> begin (* embryonic entry so insert directly and record the offset *)
                insert_string env bit x;
                fn r;
              end
            | Some off -> begin (* already present, insert pointer *)
                let off' = off land 0b11111111111111 in
                if off <> off' then raise Bad_dns_label; (* only got 14 bits for the offset *)
                let b = (0b11 lsl 14) + off' in
                append_uint16_be env b
              end
        with
          | Not_found -> begin
              (* should not be compressing since no entry in the table *)
              raise Bad_dns_label;
            end
      end else begin
        insert_string env bit x;
        fn r
      end
  | [] ->
      append_byte env 0 in
      fn t;
      let size = env.pos - start_pos in
      (* our precalculated size and the actual size better match *)
      assert(psz=size);
      size,t

let unmarshal env =
  let base_loc = env.off + env.pos in
  let start_size = env.pos in
  let rec fn acc toadd =
    let sz = unmarshal_byte env in
    let ty = sz lsr 6 in
    let cnt = sz land 0b111111 in
    match ty,cnt with
    | 0b00,0 (* eol *) ->
        (* add any bits to the unmarshal list *)
        let _ = List.fold_left2 (fun acc str off ->
          let acc = str :: acc in
          Hashtbl.add unmarshal_labels off acc;
          acc
        ) [] acc toadd in
        acc
    | 0b00,x (* lab *) ->
        let off = env.off + env.pos - 1 - !unmarshal_base in
        let str = to_string env env.pos cnt in
        skip env cnt;
        fn (str :: acc) (off :: toadd)
    | 0b11,x (* offset *) ->
        let off = (x lsl 8) + (unmarshal_byte env) in
        if off >= base_loc then raise Bad_dns_label;
        let remainder =
          try
            Hashtbl.find unmarshal_labels off
          with Not_found ->
            raise Bad_dns_label 
        in
        let _ = List.fold_left2 (fun acc str off ->
          let acc = str :: acc in
          let add = acc @ remainder in
          Hashtbl.add unmarshal_labels off add;
          acc
        ) [] acc toadd in
        List.rev_append remainder acc
    | _ -> raise Bad_dns_label in
    let res = List.rev (fn [] []) in
    let endsz = env.pos - start_size in
    endsz, res

let size (x:t) = fst x
let prettyprint t = Printf.sprintf "[%s]" (String.concat "." t)

