
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
    if k = Sdl.K.escape && v then state lor (mask_of_button Quit)
    else state
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
