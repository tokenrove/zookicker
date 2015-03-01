type t
type outcome = Cleared of int | Quit | TimeOver
val update : Input.t -> Input.t -> Time.t -> t -> t*(outcome option)
val load_level : Font.t -> int -> int -> t
val render : Tsdl.Sdl.renderer -> t -> unit
