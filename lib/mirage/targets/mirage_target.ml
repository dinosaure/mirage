open Functoria

module Key = Mirage_key

module type S = sig 

  type target

  val cast : Key.mode -> target

  val dune : Info.t -> Dune.stanza list 

  val configure : Info.t -> unit Action.t 

  val build_context : ?build_dir:Fpath.t -> Info.t -> Dune.stanza list

  val context_name : Info.t -> string

  val packages : target -> package list 

  val install : Info.t -> Install.t
end

module Solo5 : S = Mirage_target_solo5
module Unix : S = Mirage_target_unix

let choose = function
  | #Mirage_target_solo5.target -> (module Solo5 : S)
  | #Mirage_target_unix.target -> (module Unix : S)

let dune i = 
  let target = Info.get i Key.target in 
  let (module Target) = choose target in
  Target.dune i
    
let configure i = 
  let target = Info.get i Key.target in 
  let (module Target) = choose target in
  Target.configure i

let build_context ?build_dir i = 
  let target = Info.get i Key.target in 
  let (module Target) = choose target in
  Target.build_context ?build_dir i

let context_name i = 
  let target = Info.get i Key.target in 
  let (module Target) = choose target in
  Target.context_name i

let packages target = 
  let (module Target) = choose target in
  Target.(packages (cast target))

let install i = 
  let target = Info.get i Key.target in 
  let (module Target) = choose target in
  Target.install i

