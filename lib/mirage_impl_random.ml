open Functoria

[@@@warning "-32"]

type entry_points = ENTRY_POINTS
let entry_points = Type ENTRY_POINTS

let[@warning "-32"] entry_points_of_target = function
  | #Mirage_key.mode_unix  -> [ package ~sublibs:[ "main" ] "mirage-unix" ]
  | #Mirage_key.mode_solo5 -> [ package ~sublibs:[ "main" ] "mirage-solo5" ]
  | #Mirage_key.mode_xen   -> [ package ~sublibs:[ "main" ] "mirage-xen" ]

let entry_points_unix = impl @@ object
  inherit base_configurable
  method ty = entry_points
  val name = "entry-points-unix"
  method name = name
  method module_name = "Mirage_unix_main"
  method! keys = [ Mirage_key.(abstract target) ]
  method! packages =
    Mirage_key.pure
      [ package ~sublibs:[ "main" ] "mirage-unix" ]
end

let entry_points_solo5 = impl @@ object
  inherit base_configurable
  method ty = entry_points
  val name = "entry-points-solo5"
  method name = name
  method module_name = "Mirage_solo5_main"
  method! keys = [ Mirage_key.(abstract target) ]
  method! packages =
    Mirage_key.pure
      [ package ~sublibs:[ "main" ] "mirage-solo5" ]
end

let entry_points_xen = impl @@ object
  inherit base_configurable
  method ty = entry_points
  val name = "entry-points-xen"
  method name = name
  method module_name = "Mirage_xen_main"
  method! keys = [ Mirage_key.(abstract target) ]
  method! packages =
    Mirage_key.pure
      [ package ~sublibs:[ "main" ] "mirage-xen" ]
end

let default_entry_points = impl @@ object
    inherit base_configurable
    method ty = entry_points
    method name = "entry-points"
    method module_name = "OS.Main"
end

(*
  match_impl Mirage_key.(value target)
    [ `Xen, entry_points_xen
    ; `Qubes, entry_points_xen
    ; `Virtio, entry_points_solo5
    ; `Hvt, entry_points_solo5
    ; `Spt, entry_points_solo5
    ; `Muen, entry_points_solo5
    ; `Genode, entry_points_solo5 ]
  ~default:entry_points_unix
*)

type random = RANDOM
let random = Type RANDOM

let stdlib_random_conf = object
  inherit base_configurable
  method ty = entry_points @-> random
  method name = "random-stdlib"
  method module_name = "Mirage_random_stdlib.Make"
  method! packages =
    Mirage_key.pure [ package "mirage-random-stdlib" ]
  method! connect _ modname _ = Fmt.strf "%s.initialize ()" modname
end

let stdlib_random_func = impl stdlib_random_conf
let stdlib_random (entry_points : entry_points impl) = stdlib_random_func $ entry_points

(* This is to check that entropy is a dependency if "tls" is in
   the package array. *)
let enable_entropy, is_entropy_enabled =
  let r = ref false in
  let f () = r := true in
  let g () = !r in
  (f, g)

let nocrypto = impl @@ object
    inherit base_configurable
    method ty = job
    method name = "nocrypto"
    method module_name = "Nocrypto_entropy"
    method! packages =
      Mirage_key.match_ Mirage_key.(value target) @@ function
      | `Unix | `MacOSX ->
        [ package ~min:"0.5.4" ~max:"0.6.0" ~sublibs:["lwt"] "nocrypto" ]
      | _ ->
        [ package ~min:"0.5.4" ~max:"0.6.0" ~sublibs:["mirage"] "nocrypto" ;
          package ~min:"0.4.1" ~max:"0.5.0" "mirage-entropy" ]

    method! build _ = Rresult.R.ok (enable_entropy ())
    method! connect i _ _ =
      match Mirage_impl_misc.get_target i with
      | #Mirage_key.mode_xen | #Mirage_key.mode_solo5 ->
        "Nocrypto_entropy_mirage.initialize ()"
      | #Mirage_key.mode_unix -> "Nocrypto_entropy_lwt.initialize ()"
  end

let nocrypto_random_conf = object
  inherit base_configurable
  method ty = random
  method name = "random"
  method module_name = "Nocrypto.Rng"
  method! packages =
    Mirage_key.pure [ package ~min:"0.5.4" ~max:"0.6.0" "nocrypto" ]
  method! deps = [abstract nocrypto]
end

let nocrypto_random = impl nocrypto_random_conf

let default_random ?(entry_points = default_entry_points) () =
  Fmt.pr "---------- default_random -----------\n%!" ;
  Fmt.pr "%a\n%!" Functoria_graph.pp_dot (Functoria_graph.create stdlib_random_func) ;
  Fmt.pr "---------- default_random applied -----------\n%!" ;
  let stdlib_random = stdlib_random entry_points in
  Fmt.pr "%a\n%!" Functoria_graph.pp_dot (Functoria_graph.create stdlib_random) ;
  match_impl (Mirage_key.value Mirage_key.prng) [
    `Stdlib  , stdlib_random;
    `Nocrypto, nocrypto_random;
  ] ~default:stdlib_random
