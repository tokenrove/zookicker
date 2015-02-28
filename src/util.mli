val unwind : protect:('a -> unit) -> ('a -> 'b) -> 'a -> 'b
val (>>=) : [< `Error of string | `Ok of 'a ] -> ('a -> 'b) -> 'b
