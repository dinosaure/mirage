type random
type main

val random : random Functoria.typ
val main : main Functoria.typ

val default_main : main Functoria.impl

val stdlib_random : (main -> random) Functoria.impl

val default_random : ?main:main Functoria.impl -> unit -> random Functoria.impl

val nocrypto : Functoria.job Functoria.impl

val nocrypto_random : (main -> random) Functoria.impl

val is_entropy_enabled : unit -> bool
