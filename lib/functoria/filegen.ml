(*
 * Copyright (c) 2013-2020 Thomas Gazagnaire <thomas@gazagnaire.org>
 * Copyright (c) 2013-2020 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2015-2020 Gabriel Radanne <drupyog@zoho.com>
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
 *)

open Astring
open Action.Syntax

module type PROJECT = sig
  val name : string

  val version : string
end

module Make (P : PROJECT) = struct
  let lang path =
    let base, ext = Fpath.split_ext path in
    let base = Fpath.basename base in
    match (base, ext) with
    | _, (".ml" | ".mli") -> Some `OCaml
    | _, (".opam" | ".install") -> Some `Opam
    | _, ".locked" -> Some `OpamLock
    | "Makefile", _ -> Some `Make
    | ("dune" | "dune-project" | "dune-workspace"), _ -> Some `Sexp
    | _ -> None

  let headers lang =
    let line = Fmt.str "Generated by %s.%s" P.name P.version in
    match lang with
    | `Sexp -> Fmt.str ";; %s" line
    | `Opam | `Make -> Fmt.str "# %s" line
    | `OCaml -> Fmt.str "(* %s *)" line
    | `OpamLock ->
        Fmt.invalid_arg "Opam lockfiles are generated by opam-monorepo."

  let short_headers lang =
    match lang with
    | `Sexp -> Fmt.str ";; Generated by"
    | `Opam | `Make -> "# Generated by"
    | `OCaml -> "(* Generated by"
    | `OpamLock -> "synopsis: \"duniverse generated"

  let has_headers file contents =
    match Fpath.basename file with
    | "dune-project" | "dune-workspace" -> (
        let lines = String.cuts ~sep:"\n" ~empty:true (String.trim contents) in
        match List.rev lines with
        | x :: _ -> String.is_infix ~affix:(short_headers `Sexp) x
        | _ -> false )
    | _ -> (
        match lang file with
        | None -> false
        | Some lang ->
            let affix = short_headers lang in
            String.is_infix ~affix contents )

  let can_overwrite file =
    let* is_file = Action.is_file file in
    if is_file then
      let+ content = Action.read_file file in
      has_headers file content
    else Action.ok true

  let rm file =
    let* can_overwrite = can_overwrite file in
    if not can_overwrite then Action.ok () else Action.rm file

  let with_headers file contents =
    if has_headers file contents then contents
    else
      match Fpath.basename file with
      | "dune-project" | "dune-workspace" | "dune-workspace.config" ->
          Fmt.str "%s\n%s\n" contents (headers `Sexp)
      | _ -> (
          match lang file with
          | None -> Fmt.invalid_arg "%a: invalide lang" Fpath.pp file
          | Some lang -> Fmt.str "%s\n\n%s" (headers lang) contents )

  let write file contents =
    let* can_overwrite = can_overwrite file in
    if not can_overwrite then Action.ok ()
    else Action.write_file file (with_headers file contents)
end
