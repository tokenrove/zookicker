type t = {
  board: Board.t;
  mutable time_remaining: Time.t;
  mutable score: int;
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

let load_level l =
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
    | _ -> failwith "No such level"
  in
  match Board.create plan with
  | Some board ->
    {board; time_remaining = Time.of_float 300.0; score = 0;}
  | None ->
    failwith "Invalid board"

let render renderer {board} =
  Board.render renderer board
