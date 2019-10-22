type entry_points

val entry_points : entry_points Functoria.typ

val default_entry_points : entry_points Functoria.impl

type random

val random : random Functoria.typ

val stdlib_random : entry_points Functoria.impl -> random Functoria.impl

val default_random : ?entry_points:entry_points Functoria.impl -> unit -> random Functoria.impl

val nocrypto : Functoria.job Functoria.impl

val nocrypto_random : (entry_points -> random) Functoria.impl

val is_entropy_enabled : unit -> bool
