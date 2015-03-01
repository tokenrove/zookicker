open Tsdl

type t
val load: Sdl.renderer -> string -> t
val free: t -> unit
val with_font: Sdl.renderer -> string -> (t -> 'a) -> 'a
val render_line: Sdl.renderer -> t -> (int*int) -> (int*int*int) -> string -> unit
val line_height: t -> int
val text_width: t -> string -> int
