
type t = int
type button =
    Quit
  | Left
  | Right
  | Up
  | Down
  | Kick

let mask_of_button = function
  | Left -> 1
  | Right -> 2
  | Up -> 4
  | Down -> 8
  | Kick -> 16
  | Quit -> 32

let empty = 0

open Tsdl

let update e initial_state =
  let handle_key state v =
    let k = Sdl.Event.(get e keyboard_keycode) in
    let m = if k = Sdl.K.escape then (mask_of_button Quit)
      else if k = Sdl.K.left then (mask_of_button Left)
      else if k = Sdl.K.right then (mask_of_button Right)
      else if k = Sdl.K.up then (mask_of_button Up)
      else if k = Sdl.K.down then (mask_of_button Down)
      else if k = Sdl.K.space then (mask_of_button Kick)
      else 0
    in
    if v then state lor m else state land (lnot m)
  in
  let rec loop state =
    if Sdl.poll_event (Some e) then begin
      loop
        (match Sdl.Event.(enum (get e typ)) with
         | `Quit -> state lor (mask_of_button Quit)
         | `Key_down -> handle_key state true
         | `Key_up -> handle_key state false
         | _ -> state)
    end else state
  in loop initial_state

let is_pressed button state = 0 <> (state land (mask_of_button button))
