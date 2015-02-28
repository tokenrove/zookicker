type t = {
  board: Board.t;
  mutable time_remaining: Time.t;
  mutable score: int;
  font: Font.t;
}

type outcome = Cleared | Quit | TimeOver

let update input old_input dt ({board; time_remaining} as game) =
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
  let outcome =
    if Input.is_pressed Input.Quit input then Some Quit
    else if Board.is_complete board then Some Cleared
    else if time_remaining <= 0. then Some TimeOver
    else None
  in
  (game, outcome)

let load_level font l =
  let plan = match l with
    | 0 -> "
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
"
    | 1 -> "
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
"
    | 2 -> "
****************
****************
*...********..@*
***.********.***
***.12.......***
***1..2314565***
***7..8319657***
***.78....97.***
***.********.***
*...********...*
****************
****************
"
    | _ -> failwith "No such level"
  in
  match Board.create plan with
  | Some board ->
    {board; time_remaining = Time.of_float 300.0; score = 0; font}
  | None ->
    failwith "Invalid board"

open Tsdl
let (>>=) = Util.(>>=)

let render renderer {board; font; score; time_remaining} =
  Sdl.set_render_draw_color renderer 0 0 0 0xff >>= fun () ->
  Sdl.render_clear renderer >>= fun () ->
  Board.render renderer board;
  (* XXX should use buffers for these strings *)
  Font.render_line renderer font (300, 400) (255,0,0) (Printf.sprintf "%08d" score);
  let (mins, secs) = truncate (time_remaining /. 60.), (truncate time_remaining) mod 60 in
  Font.render_line renderer font (0, 400) (255,255,0) (Printf.sprintf "%d:%02d" mins secs);
  Sdl.render_present renderer
