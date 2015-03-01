val unwind : protect:('a -> unit) -> ('a -> 'b) -> 'a -> 'b
val (>>=) : [< `Error of string | `Ok of 'a ] -> ('a -> 'b) -> 'b
val with_music : string -> (Tsdl_mixer.Mixer._music Ctypes.structure Ctypes.ptr -> 'a) -> 'a
