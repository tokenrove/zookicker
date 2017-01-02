let unwind ~(protect:'a -> unit) f x =
  try let y = f x in protect x; y
  with e -> protect x; raise e

open Tsdl
open Result
let (>>=) o f =
  match o with | Error (`Msg e) -> failwith (Printf.sprintf "Error %s" e)
               | Ok a -> f a

open Tsdl_mixer
let with_music path f =
  Mixer.load_mus path >>= fun music ->
  Mixer.play_music music (-1) |> ignore;
  unwind ~protect:(fun music ->
      (* XXX make sure the music isn't still playing? *)
      Mixer.halt_music () |> ignore;
      Mixer.free_music music)
    f music

let modify_rect r x y w h =
  Sdl.Rect.set_x r x;
  Sdl.Rect.set_y r y;
  Sdl.Rect.set_w r w;
  Sdl.Rect.set_h r h;
  r
