open TestSimple
open Board

let board_created_from_tricky_kick_level_1_reports_correct_number_of_pairs () =
  is (match create "
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
  | Some board -> Some (is_complete board, pairs_remaining board)
  | None -> None)
    (Some (false, 6))
    "Example board is valid"

let board_created_reports_correct_number_of_pairs () =
  is (match create "
@4..
1..1
.4..
" with
  | Some board -> Some (pairs_remaining board)
  | None -> None)
    (Some 2)
    "Board created reports correct number of pairs"

let board_created_with_odd_number_of_beasts_is_not_valid () =
  ok (match create "
@...
1..1
.4..
" with
     | Some _ -> false | None -> true)
    "Board created with odd number of beasts is not valid"

let board_created_without_player_is_not_valid () =
  ok (match create "
....
11.2
.2..
" with
     | Some _ -> false | None -> true)
    "Board created without player is not valid"

(* Not sure if this should be valid or not. *)
let board_created_with_no_beasts_is_complete () =
  is (match create "
****
*.@*
****
" with
     | Some it -> Some (pairs_remaining it, is_complete it)
     | None -> None)
    (Some (0, true))
    "Board created with no beasts is complete"

let str_of_direction = function
  | Left -> "left"
  | Right -> "right"
  | Up -> "up"
  | Down -> "down"

exception Perpetual_motion

let update_until_unchanged board =
  let max_iterations = ref 100 in
  while update (Time.of_int32 16l) board do
    decr max_iterations;
    if !max_iterations <= 0 then raise Perpetual_motion;
  done

let player_can_move direction =
  let expected = match direction with
    | Left -> (-1,0)
    | Right -> (1,0)
    | Up -> (0,-1)
    | Down -> (0,1)
  in
  is (match create "
*****
*...*
*.@.*
*...*
*****
" with
     | Some it ->
       let (x,y) = locate_player it in
       move direction it;
       update_until_unchanged it;
       let (x',y') = (locate_player it) in
       Some (x'-x, y'-y)
     | None -> None)
    (Some expected)
    ("Player can move " ^ (str_of_direction direction))

let player_cannot_move_through_inanimate_objects direction =
  is (match create "
.....
..*..
.*@*.
..*..
.....
" with
     | Some it ->
       let (x,y) = locate_player it in
       move direction it;
       update_until_unchanged it;
       let (x',y') = (locate_player it) in
       Some (x'-x, y'-y)
     | None -> None)
    (Some (0,0))
    ("Player cannot move through inanimate objects")

let player_cannot_move_through_beasts direction =
  is (match create "
.....
..1..
.1@1.
..1..
.....
" with
     | Some it ->
       let (x,y) = locate_player it in
       move direction it;
       update_until_unchanged it;
       let (x',y') = (locate_player it) in
       Some (x'-x, y'-y)
     | None -> None)
    (Some (0,0))
    ("Player cannot move through beasts")

let player_wraps_around_world direction =
  let plan = match direction with
    | Left -> "@.."
    | Right -> "..@"
    | Up -> "
.@.
...
..."
    | Down -> "
...
...
.@." in
  is (match create plan with
      | Some it ->
        let (x,y) = locate_player it in
        move direction it;
        update_until_unchanged it;
        let (x',y') = (locate_player it) in
        Some ((x'-x)*(x'-x) + (y'-y)*(y'-y))
      | None -> None)
    (Some 2)
    ("Player wraps around the world ("^(str_of_direction direction)^")")

(*
 * let player_takes_time_to_complete_move () =
 *   is (match create "@." with
 *       | Some it ->
 *         let (x,_) = locate_player it in
 *         move Right it;
 *         let (x',_) = locate_player it in
 *         update (Time.of_int32 16l) it;
 *         let (x'',_) = locate_player it in
 *         Some (x = x', x <> x'')
 *       | None -> None)
 *     (Some (true, true))
 *     "Player takes time to complete move"
 *)

let kicking_one_beast_unobstructed_stops_at_obstacle () =
  is (match create "@1..*1" with
     | Some it ->
       let (x,y) = locate_player it in
       move Right it;
       kick it;
       update_until_unchanged it;
       move Right it;
       update_until_unchanged it;
       move Right it;
       update_until_unchanged it;
       move Right it;
       update_until_unchanged it;
       let (x',y') = (locate_player it) in
       Some (x'-x, y'-y)
     | None -> None)
    (Some (2,0))
    ("Kicking one beast unobstructed stops at obstacle")

let kicked_beast_with_no_obstruction_wraps_til_player () =
  is (match create "
.....1
.1@..." with
      | Some it ->
        let (x,y) = locate_player it in
        move Left it;
        kick it;
        update_until_unchanged it;
        move Right it;
        update_until_unchanged it;
        let (x',y') = (locate_player it) in
        Some (x'-x, y'-y)
      | None -> None)
    (Some (0,0))
    ("Kicking beast with no obstruction wraps til player")

let kicking_two_beasts_causes_second_to_move_til_obstacle () =
  is (match create ".@11.." with
      | Some it ->
        let (x,y) = locate_player it in
        move Right it;
        kick it;
        update_until_unchanged it;
        move Left it;
        update_until_unchanged it;
        let (x',y') = (locate_player it) in
        Some (x'-x, y'-y)
      | None -> None)
    (Some (1,0))
    ("Kicking two beasts causes second to move til obstacle")

let kicking_beast_into_matching_beast_eliminates_it () =
  is (match create "@1.1" with
      | Some it ->
        move Right it;
        kick it;
        update_until_unchanged it;
        Some (is_complete it)
      | None -> None)
    (Some true)
    ("Kicking beast into matching beast eliminates it")

let kicking_beast_through_beast_into_matching_beast_eliminates_one_pair () =
  is (match create "@11.11" with
      | Some it ->
        move Right it;
        kick it;
        update_until_unchanged it;
        Some (pairs_remaining it)
      | None -> None)
    (Some 1)
    ("Kicking beast through beast into matching beast eliminates one pair")

let cannot_kick_beast_through_solid_object () =
  is (match create "@*1..1" with
      | Some it ->
        move Right it;
        kick it;
        update_until_unchanged it;
        Some (is_complete it)
      | None -> None)
    (Some false)
    ("Cannot kick beast through solid object")

let kicking_three_beasts_has_no_effect () =
  is (match create "@111.1" with
      | Some it ->
        move Right it;
        kick it;
        update_until_unchanged it;
        Some (pairs_remaining it)
      | None -> None)
    (Some 2)
    ("Kicking three beasts has no effect")

let kicking_two_beasts_obstructed_has_no_effect () =
  is (match create "@11*" with
      | Some it ->
        move Right it;
        kick it;
        update_until_unchanged it;
        Some (pairs_remaining it)
      | None -> None)
    (Some 1)
    ("Kicking two beasts obstructed has no effect")

let kicking_one_beast_obstructed_has_no_effect () =
  is (match create "@1*.1" with
      | Some it ->
        move Right it;
        kick it;
        update_until_unchanged it;
        Some (pairs_remaining it)
      | None -> None)
    (Some 1)
    ("Kicking one beast obstructed has no effect")

  let () =
  plan 0;
  board_created_from_tricky_kick_level_1_reports_correct_number_of_pairs ();
  board_created_reports_correct_number_of_pairs ();
  board_created_with_odd_number_of_beasts_is_not_valid ();
  board_created_without_player_is_not_valid ();
  (* board_created_with_no_beasts_is_not_valid (); *)
  board_created_with_no_beasts_is_complete ();
  let directions = [Up; Down; Left; Right] in
  List.iter player_can_move directions;
  List.iter player_cannot_move_through_inanimate_objects directions;
  List.iter player_cannot_move_through_beasts directions;
  List.iter player_wraps_around_world directions;
  (* player_takes_time_to_complete_move (); *)
  kicking_one_beast_unobstructed_stops_at_obstacle ();
  kicking_two_beasts_causes_second_to_move_til_obstacle ();
  kicked_beast_with_no_obstruction_wraps_til_player ();
  cannot_kick_beast_through_solid_object ();
  kicking_three_beasts_has_no_effect ();
  kicking_two_beasts_obstructed_has_no_effect ();
  kicking_one_beast_obstructed_has_no_effect ();
  kicking_beast_into_matching_beast_eliminates_it ();
  exit 0
