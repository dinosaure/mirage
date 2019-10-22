open Functoria

let () = Printexc.record_backtrace true

type random = RANDOM
let random = Type RANDOM

type main = MAIN
let main = Type MAIN

let main_target = function
  | #Mirage_key.mode_unix -> [ package ~sublibs:[ "main" ] "mirage-unix" ]
  | #Mirage_key.mode_solo5 -> [ package ~sublibs:[ "main" ] "mirage-solo5" ]
  | #Mirage_key.mode_xen -> [ package ~sublibs:[ "main" ] "mirage-xen" ]

let main_config = object
  inherit base_configurable
  method ty = main
  method name = "mirage-main-unix"
  method module_name = "Mirage_main"
  method! packages =
    Mirage_key.map (fun mode ->  package "mirage-os-shim" :: main_target mode) (Mirage_key.value Mirage_key.target)
end

let stdlib_random_config = object
  inherit base_configurable
  method ty = main @-> random
  method name = "stdlib-random"
  method module_name = "Mirage_random_stdlib.Make"
  method! packages =
    Mirage_key.pure [ package ~max:"0.1.0" "mirage-random-stdlib" ]
  method! connect _ modname _ = Fmt.strf "%s.initialize ()" modname
end

let default_main = impl main_config
let stdlib_random = impl stdlib_random_config

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
  method ty = main @-> random
  method name = "random"
  method module_name = "Nocrypto.Rng"
  method! packages =
    Mirage_key.pure [ package ~min:"0.5.4" ~max:"0.6.0" "nocrypto" ]
  method! deps = [abstract nocrypto]
end

let nocrypto_random = impl nocrypto_random_conf

let default_random ?(main = default_main) () =
  let stdlib_random = stdlib_random $ main in
  match_impl (Mirage_key.value Mirage_key.prng) [
    `Stdlib  , stdlib_random;
    `Nocrypto, nocrypto_random $ main;
  ] ~default:stdlib_random
