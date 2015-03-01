type t = {
  board: Board.t;
  mutable time_remaining: Time.t;
  mutable score: int;
  mutable pairs_kicked: int;
  font: Font.t;
  rect: Tsdl.Sdl.rect;            (* triple yuck; see HACKING.org *)
}

type outcome = Cleared of int | Quit | TimeOver

let update input old_input dt ({board} as game) =
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
  let score_pts _ =
    game.pairs_kicked <- game.pairs_kicked + 1;
    game.score <- game.score + (10 * game.pairs_kicked)
  in
  Board.update dt board score_pts |> ignore;
  game.time_remaining <- game.time_remaining -. dt;
  let outcome =
    if Input.is_pressed Input.Quit input then Some Quit
    else if Board.is_complete board then Some (Cleared game.score)
    else if game.time_remaining <= 0. then Some TimeOver
    else None
  in
  (game, outcome)

let levels = [| "
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
", 90., "assets/level0.ogg"; "
****************
****************
****************
*******..*******
*****....@.*****
*****.1234.*****
*****.4321.*****
*****......*****
*******..*******
****************
****************
****************
", 150., "assets/level1.ogg"; "
****************
****************
*...********..@*
***.********.***
***.12.......***
***1..2314564***
***7..8319657***
***.78....97.***
***.********.***
*...********...*
****************
****************
", 300., "assets/level2.ogg" |]

let with_level font previous_score l f =
  let (plan, time_limit, music_path) =
    if l >= 0 && l < (Array.length levels) then
      levels.(l)
    else failwith "No such level"
  in
  match Board.create plan with
  | Some board ->
    Util.with_music music_path @@ fun _ ->
    f {board;
       time_remaining = Time.of_float time_limit;
       score = previous_score;
       pairs_kicked = 0;
       rect = Tsdl.Sdl.Rect.create 0 0 0 0;
       font}
  | None ->
    failwith "Invalid board"

open Tsdl
let (>>=) = Util.(>>=)

let render renderer {board; font; score; time_remaining; rect} =
  Sdl.set_render_draw_color renderer 0 0 0 0xff >>= fun () ->
  Sdl.render_clear renderer >>= fun () ->
  Board.render renderer board rect;
  (* XXX should use buffers for these strings *)
  Font.render_line renderer font (600, 30) (255,255,255) (Printf.sprintf "%08d" score);
  let (mins, secs) = truncate (time_remaining /. 60.), (truncate time_remaining) mod 60 in
  let color = if mins = 0 then (160,0,0) else if mins = 1 then (255,255,0) else (0,255,0) in
  Font.render_line renderer font (30, 30) color (Printf.sprintf "%d:%02d" mins secs);
  Sdl.render_present renderer
