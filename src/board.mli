(* This is deliberately an imperative interface; we want to avoid
   consing in the inner game loop *)
type t
type direction = Up | Down | Left | Right

val create : string -> t option
val update : Time.t -> t -> bool
val move : direction -> t -> unit
val kick : t -> unit
val is_valid : t -> bool
val is_complete : t -> bool
val pairs_remaining : t -> int

val locate_player : t -> int*int
