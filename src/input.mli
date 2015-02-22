(* Provides a virtual controller interface *)

type t
type button =
    Quit
  | Left
  | Right
  | Up
  | Down
  | Kick

val empty : t
val update : Tsdl.Sdl.event -> t -> t
val is_pressed : button -> t -> bool
