let unwind ~(protect:'a -> unit) f x =
  try let y = f x in protect x; y
  with e -> protect x; raise e

open Tsdl
let (>>=) o f =
  match o with | `Error e -> failwith (Printf.sprintf "Error %s" e)
               | `Ok a -> f a
