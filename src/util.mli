val unwind : protect:('a -> unit) -> ('a -> 'b) -> 'a -> 'b
val (>>=) : 'a Tsdl.Sdl.result -> ('a -> 'b) -> 'b
val with_music : string -> (Tsdl_mixer.Mixer.music -> 'a) -> 'a
val modify_rect : Tsdl.Sdl.rect -> int -> int -> int -> int -> Tsdl.Sdl.rect
