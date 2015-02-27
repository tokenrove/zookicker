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

let game_fn input old_input dt board =
  let debounced i =
    Input.is_pressed i input &&
    not (Input.is_pressed i old_input)
  in
  List.iter (fun (d,i) -> if debounced i then Board.move d board)
    [Board.Left,Input.Left;
     Board.Right,Input.Right;
     Board.Up,Input.Up;
     Board.Down,Input.Down];
  if debounced Input.Kick then Board.kick board;
  Board.update dt board |> ignore;
  (board, (Input.is_pressed Input.Quit input) || Board.is_complete board)

let timed_event_loop target_fps render_fn game_fn renderer initial_game_value =
  let minimum_frame_length = (Int32.div 1000l (Int32.of_int target_fps)) in
  let minimum_update_length = 2l in
  let event = Sdl.Event.create () in
  let rec loop last_t dt game input =
    let input' = Input.update event input in
    (* we could be returning a closure here instead of a game state, but
       i wanted to avoid the chance that this would cons *)
    let (game', quit) =
      if dt < minimum_update_length then (game, false)
      else game_fn input' input (Time.of_int32_ms dt) game
    in

    render_fn renderer game';

    let t = Sdl.get_ticks () in
    let tick_diff = (Int32.sub t last_t) in
    if tick_diff < minimum_frame_length then begin
      Sdl.delay (Int32.sub minimum_frame_length tick_diff)
    end;

    if not quit then
      loop t (max tick_diff minimum_frame_length) game' input'
  in
  loop (Sdl.get_ticks ()) 0l initial_game_value Input.empty

let () =
  with_sdl (fun _ renderer ->
      match Board.create "
****************
****************
*.*****..*****.*
*.1***....***2.*
*...*....@.*...*
*...3..12..3...*
*...3..12..3...*
*...*......*...*
*.1***....***2.*
*.*****..*****.*
****************
****************
" with
      | Some board ->
        timed_event_loop 60 Board.render game_fn renderer board
      | None ->
        Printf.fprintf stderr "Failed to load default board.  Sorry.\n") ();
  exit 0
