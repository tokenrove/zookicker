type t
type outcome = Cleared | Quit | TimeOver
val update : Input.t -> Input.t -> Time.t -> t -> t*(outcome option)
val load_level : int -> t
val render : Tsdl.Sdl.renderer -> t -> unit
