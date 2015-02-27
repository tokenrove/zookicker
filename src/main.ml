let window_title = "Zookicker"

let unwind ~(protect:'a -> unit) f x =
  try let y = f x in protect x; y
  with e -> protect x; raise e

open Tsdl

let (>>=) o f =
  match o with | `Error e -> failwith (Printf.sprintf "Error %s" e)
               | `Ok a -> f a

let with_sdl f =
  Sdl.init Sdl.Init.everything >>= fun () ->
  Sdl.create_window ~w:640 ~h:480 window_title Sdl.Window.opengl >>= fun w ->
  Sdl.create_renderer w >>= fun r ->
  unwind ~protect:(fun () ->
      Sdl.destroy_window w;
      Sdl.quit ()) (fun () -> f w r)

let timed_event_loop target_fps render_fn game_fn renderer initial_game_value =
  let minimum_frame_length = (Int32.div 1000l (Int32.of_int target_fps)) in
  let minimum_update_length = 2l in
  let event = Sdl.Event.create () in
  let rec loop last_t dt game input =
    let input' = Input.update event input in
    (* we could be returning a closure here instead of a game state, but
       i wanted to avoid the chance that this would cons *)
    let (game', outcome) =
      if dt < minimum_update_length then (game, None)
      else game_fn input' input (Time.of_int32_ms dt) game
    in

    render_fn renderer game';

    let t = Sdl.get_ticks () in
    let tick_diff = (Int32.sub t last_t) in
    if tick_diff < minimum_frame_length then begin
      Sdl.delay (Int32.sub minimum_frame_length tick_diff)
    end;

    match outcome with
    | None -> loop t (max tick_diff minimum_frame_length) game' input'
    | Some v -> v
  in
  loop (Sdl.get_ticks ()) 0l initial_game_value Input.empty

let () =
  with_sdl (fun _ renderer ->
      let levels = [0] in
      let rec loop ls =
        match ls with
        | [] -> Printf.printf "You win.\n"
        | l::ls' ->
          match timed_event_loop 60 Game.render Game.update renderer (Game.load_level l) with
          | Game.Cleared -> loop ls'
          | Game.Quit | Game.TimeOver -> Printf.printf "You lose.\n"
      in
      loop levels)
    ();
  exit 0
