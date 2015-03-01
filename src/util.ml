let unwind ~(protect:'a -> unit) f x =
  try let y = f x in protect x; y
  with e -> protect x; raise e

open Tsdl
let (>>=) o f =
  match o with | `Error e -> failwith (Printf.sprintf "Error %s" e)
               | `Ok a -> f a

open Tsdl_mixer
let with_music path f =
  match Mixer.load_mus path with
  | None -> failwith (Printf.sprintf "Failed to load %s" path)
  | Some music ->
    Mixer.play_music music (-1) |> ignore;
    unwind ~protect:(fun music ->
        (* XXX make sure the music isn't still playing? *)
        Mixer.halt_music () |> ignore;
        Mixer.free_music music)
      f music
