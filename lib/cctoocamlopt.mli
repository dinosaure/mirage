type error =
  [ `Cmd of [ `Exn | `Parse | `Term ] | `Msg of string ]

val run_with_binary : string array -> (string list, error) result
val run : ?binary:string -> string array -> (string list, error) result
