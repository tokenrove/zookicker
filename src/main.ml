let window_title = "Zookicker"

open Tsdl
open Tsdl_image
open Tsdl_mixer
let (>>=) = Util.(>>=)

let display_width, display_height = 1024, 768

let with_sdl fullscreen_p f =
  Sdl.init Sdl.Init.everything >>= fun () ->
  assert ((Image.init Image.Init.png) = Image.Init.png);
  assert ((Mixer.init Mixer.Init.ogg) = Mixer.Init.ogg);
  (* XXX audio should be optional *)
  assert (0 = (Mixer.open_audio 44100 Sdl.Audio.s16 2 4096));
  (if fullscreen_p then
    Sdl.create_window_and_renderer ~w:0 ~h:0 Sdl.Window.fullscreen_desktop
  else
    Sdl.create_window_and_renderer ~w:display_width ~h:display_height Sdl.Window.windowed)
  >>= fun (window, renderer) ->
  Sdl.render_set_logical_size renderer display_width display_height >>= fun () ->
  Util.unwind ~protect:(fun () ->
      Sdl.destroy_window window;
      Image.quit ();
      Mixer.quit ();
      Sdl.quit ()) (fun () -> f window renderer) ()

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

    let tick_diff = (Int32.sub (Sdl.get_ticks ()) last_t) in
    if tick_diff < minimum_frame_length then begin
      Sdl.delay (Int32.sub minimum_frame_length tick_diff)
    end;
    let t = Sdl.get_ticks () in
    let tick_diff = (Int32.sub t last_t) in

    match outcome with
    | None -> loop t tick_diff game' input'
    | Some v -> v
  in
  loop (Sdl.get_ticks ()) 0l initial_game_value Input.empty

let with_music path f =
  match Mixer.load_mus path with
  | None -> failwith (Printf.sprintf "Failed to load %s" path)
  | Some music ->
    Mixer.play_music music (-1);
    Util.unwind ~protect:(fun music ->
        (* XXX make sure the music isn't still playing? *)
        Mixer.free_music music)
      f music

(* returns true if we want to play; false if we quit *)
let title_screen renderer logo_font =
  let render renderer () =
    Sdl.set_render_draw_color renderer 214 160 53 0xff >>= fun () ->
    Sdl.render_clear renderer >>= fun () ->
    Font.render_line renderer logo_font (50, 100) (255,255,255) "ZOOKICKER";
    Font.render_line renderer logo_font (50, 200) (255,255,255) "by loafwarden games";
    Font.render_line renderer logo_font (420, 500) (214,80,160) "press space";
    Sdl.render_present renderer
  in
  let update input old_input dt () =
    ((),
     if Input.is_pressed Input.Quit input then Some false
     else if Input.is_pressed Input.Kick input then Some true
     else None)
  in
  with_music "assets/title.ogg" @@ fun _ ->
  timed_event_loop 60 render update renderer ()

let end_credits renderer osd_font =
  Font.with_font renderer "assets/credits"
    (fun font ->
       let scroll = ref 1500. in
       let credits = List.rev
           ["ZooKicker";
            ""; "";
            "Written by tokenrove for #1GAM February 2015";
            "In great haste, alas; however humble, though, it is a game,";
            "and perhaps next month's will be greater.";
            "";
            "Greets to Retsyn, Doggyfred, Roddy, Obliviax, Kshred";
            "Apologies to Alfa System; this is unabashedly a clone of";
            "their great PC-Engine game, Tricky Kick";
            "";
            "";
            "tokenrove / <julian@cipht.net>";
            "";
            "See you next!"] in
       let render renderer () =
         Sdl.set_render_draw_color renderer 0 30 53 0xff >>= fun () ->
         Sdl.render_clear renderer >>= fun () ->
         let y = (truncate !scroll) - 800 in
         if y > -(Font.line_height osd_font) then
           Font.render_line renderer osd_font (350, y) (255,0,0) "You Win!";
         List.fold_left (fun y line ->
             let x = (display_width-(Font.text_width font line))/2 in
             if y > 0 then
               Font.render_line renderer font (x, y - (Font.line_height font)) (240,240,240) line;
             y - (Font.line_height font)) (truncate !scroll) credits
         |> ignore;
         Sdl.render_present renderer
       in
       let update input old_input dt () =
         if !scroll > 180. then scroll := !scroll -. (50. *. dt);
         ((),
          if Input.is_pressed Input.Quit input then Some ()
          else if Input.is_pressed Input.Kick input then Some ()
          else None)
       in
       with_music "assets/credits.ogg" @@ fun _ ->
       timed_event_loop 60 render update renderer ())

let game_over renderer font =
  let render renderer () =
    Sdl.set_render_draw_color renderer 0 30 53 0xff >>= fun () ->
    Sdl.render_clear renderer >>= fun () ->
    Font.render_line renderer font (350, 300) (255,0,0) "You Lose";
    Sdl.render_present renderer
  in
  let update input old_input dt () =
    ((),
     if Input.is_pressed Input.Quit input then Some ()
     else if Input.is_pressed Input.Kick input then Some ()
     else None)
  in
  with_music "assets/gameover.ogg" @@ fun _ ->
  timed_event_loop 60 render update renderer ()

let rec outer_game_loop renderer osd_font levels score =
  match levels with
  | [] -> end_credits renderer osd_font
  | l::ls' ->
    let open Game in
    match
      with_level osd_font score l @@ fun level ->
      timed_event_loop 60 render update renderer level
    with
    | Cleared score -> outer_game_loop renderer osd_font ls' score
    | Quit | TimeOver -> game_over renderer osd_font

(* XXX make sure we know how to find our data *)
let ensure_working_directory () =
  Printf.printf "We are in %s; We should be in the same directory as: %s\n" (Sys.getcwd ()) Sys.executable_name;
  assert (Sys.is_directory "assets")

let () =
  Gc.create_alarm (fun () ->
      Printf.fprintf stderr "GC happening; %f allocated\n" (Gc.allocated_bytes ());
      flush stderr)
  |> ignore;
  ensure_working_directory ();
  let fullscreen_p =
    Array.length Sys.argv > 1 && Sys.argv.(1) = "-f"
  in
  with_sdl fullscreen_p (fun _ renderer ->
      let levels = [0; 1; 2] in
      let osd_font = Font.load renderer "assets/osd" in
      let logo_font = Font.load renderer "assets/logofont" in
      while title_screen renderer logo_font do
        outer_game_loop renderer osd_font levels 0
      done);
  exit 0
