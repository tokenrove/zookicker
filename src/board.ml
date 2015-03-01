type direction = Up | Down | Left | Right

(* Physics in Tricky Kick is very simple.  A beast is either moving or
   not. *)
type velocity = Stationary
              | Moving of direction

type beast =
  {
    kind: int;
    offset: float; (* how far we are from the center of the current tile, in tiles *)
    velocity: velocity;
  }

type player =
  {
    facing: direction;
    offset: float; (* how far we are from the center of the current tile, in tiles *)
  }

type tile =
  | Floor
  | Solid of int
  | Beast of beast
  | Player of player

type board = tile array

type t =
  {
    width : int;
    height : int;
    mutable player_pos : int*int;
    mutable current : board;
  }

(* create a board from a crude ascii representation, for testing. *)
let create s =
  let n = String.length s in
  let is_whitespace c = c = ' ' || c = '\t' || c = '\n' in
  let rec skip_initial_whitespace i =
    if is_whitespace s.[i] then
      skip_initial_whitespace (succ i)
    else
      read_first_line i i
  and read_first_line start i =
    let i' = succ i in
    if i >= n || s.[i] = '\n' then
      let w = i-start in
      read_another_line i' w [String.sub s start w] i'
    else
      read_first_line start i'
  and read_another_line start width lines i =
    let i' = succ i in
    if i >= n || s.[i] = '\n' then
      if i-start == width then
        read_another_line i' width ((String.sub s start width)::lines) i'
      else
        (width, List.rev lines)
    else
      read_another_line start width lines i'
  in
  let (width,lines) = skip_initial_whitespace 0 in
  let height = List.length lines in
  if width = 0 || height = 0 then
    None
  else
    let board = Array.make (width*height) Floor in
    let tileify = function
      | '.' -> Floor
      | '@' -> Player {facing=Down; offset=0.}
      | '1'..'9' as c -> Beast {kind=(Char.code c)-(Char.code '0');
                                offset=0.;
                                velocity=Stationary}
      | c -> Solid (Char.code c)
    in
    let player_pos = ref None in
    let p j line =
      for i = 0 to width-1 do
        let tile = tileify line.[i] in
        board.(i+j*width) <- tile;
        match (tile, !player_pos) with
        | (Player _, None) ->
          player_pos := Some (i,j)
        | (Player _, Some (x,y)) ->
          failwith "Too many players"
        | _ -> ();
      done
    in
    try
      List.iteri p lines;
      match !player_pos with
      | Some player_pos ->
        Some {width; height; player_pos; current = board}
      | None ->
        failwith "No player"
    with | Failure _ -> None

let dump_ascii {width; height; current} =
  let char_of_tile = function
    | Floor -> '.'
    | Solid _ -> '*'
    | Player _ -> '@'
    | Beast {kind} -> Char.chr ((Char.code '0')+kind)
  in
  let b = Buffer.create ((width+1)*height) in
  for j = 0 to height-1 do
    for i = 0 to width-1 do
      Buffer.add_char b (char_of_tile current.(j*width+i))
    done;
    Buffer.add_char b '\n'
  done;
  Buffer.contents b

let unit_delta_of_direction = function
  | Left -> (-1,0)
  | Right -> (1,0)
  | Up -> (0,-1)
  | Down -> (0,1)

let get_adjacent_position {width; height;} direction (x,y) =
  assert (x >= 0 && x < width && y >= 0 && y < height);
  let (dx,dy) = unit_delta_of_direction direction in
  (width+x+dx) mod width, (height+y+dy) mod height

let update_tile dt i j ({width; height; current} as board) score_points =
  let player_speed = 5. in
  let beast_speed = 6. in
  let sink v = copysign (max (abs_float(v) -. (dt *. player_speed)) 0.) v in
  let integrate_velocity o = o -. (dt *. beast_speed) in
  match current.(i+j*width) with
  | Beast {velocity=Stationary} | Solid _ | Floor -> false
  | Player {facing; offset} ->
    (* to update the player, we ease their position towards the center of the tile *)
    current.(i+j*width) <- Player {facing; offset=(sink offset)};
    false
  | Beast ({kind=our_kind; offset; velocity=Moving direction} as beast) ->
    (* apply velocity to position *)
    let offset' = integrate_velocity offset in
    if offset' > -0.5 then begin
      current.(i+j*width) <- Beast {beast with offset=offset'};
      true
    end else begin
      let (i',j') = get_adjacent_position board direction (i,j) in
      match current.(i'+j'*width) with
      | Floor ->
        current.(i'+j'*width) <- Beast {beast with offset=0.5 +. offset};
        current.(i+j*width) <- Floor;
        true
      | Beast {kind=other_kind} when our_kind=other_kind ->
        current.(i+j*width) <- Floor;
        current.(i'+j'*width) <- Floor;
        score_points our_kind;
        true
      | Beast _ | Solid _ | Player _ ->
        current.(i+j*width) <- Beast {beast with offset=0.; velocity=Stationary};
        true
    end

let update dt board score_points =
  let {width; height; current} = board in
  let changed = ref false in
  for j = 0 to height-1 do
    for i = 0 to width-1 do
      if update_tile dt i j board score_points then
        changed := true;
    done
  done;
  !changed

let pairs_remaining_internal current =
  let f (bits,count) tile = match tile with
    | Beast {kind} ->
      let b = 1 lsl kind in
      let bits' = bits lxor b in
      (bits',
       if 0 != (bits land b) then
         count+1
       else
         count)
    | _ -> (bits,count)
  in
  Array.fold_left f (0,0) current

let is_valid {current} =
  let (bits,_) = pairs_remaining_internal current in
  0 = bits

let pairs_remaining {current} =
  let (_,count) = pairs_remaining_internal current in count

let is_complete board =
  0 == pairs_remaining board

let locate_player {player_pos} =
  player_pos

let is_player_ready_to_move {offset} = abs_float(offset) < epsilon_float

let move facing ({player_pos=(x,y); width; current} as board) =
  let tile = current.(x+y*width) in
  let (x',y') = get_adjacent_position board facing (x,y) in
  match tile with
  | Player player ->
    if is_player_ready_to_move player then
      if current.(x'+y'*width) = Floor then begin
        current.(x'+y'*width) <- Player {facing; offset=1.0};
        current.(x+y*width) <- Floor;
        board.player_pos <- (x',y')
      end else current.(x+y*width) <- Player {player with facing}
  | _ -> failwith "I was told there would be a player at this position"

(* if a kick is applied to an animal:
  if the following square is empty, ball is this animal
  if the following square is an animal and the square after that is
    empty, ball is that animal
  if there's a ball, it proceeds along movable squares
  if it comes to rest at another animal of the same type, and it
    travelled more than one square, both animals are removed
*)
let with_ball (x,y) direction ({width; current} as board) fn =
  let adjacent_to (x,y) = get_adjacent_position board direction (x,y) in
  let (cx,cy) = adjacent_to (x,y) in
  match current.(cx+cy*width) with
  | Floor | Solid _ | Player _ -> ()
  | Beast candidate ->
    let (nx,ny) = adjacent_to (cx,cy) in
    match current.(nx+ny*width) with
    | Floor | Solid _ | Player _ ->
      current.(cx+cy*width) <- Beast (fn candidate)
    | Beast neighbor ->
      let (fx,fy) = adjacent_to (nx,ny) in
      match current.(fx+fy*width) with
      | Floor -> current.(nx+ny*width) <- Beast (fn neighbor)
      | Solid _ | Player _ | Beast _ -> ()

let kick ({player_pos=(x,y); width; current} as board) =
  let tile = current.(x+y*width) in
  match tile with
  | Player ({facing} as player) ->
    if is_player_ready_to_move player then
      with_ball (x,y) facing board
        (fun beast -> {beast with velocity = Moving facing})
  | _ -> failwith "I was told there would be a player at this position"


(* I was thinking of abstracting the presentation of the board out a
   bit, possibly by parameterizing this module by a "presenter" module.
   For now, though, we'll keep things simple and concrete. *)
open Tsdl
let (>>=) = Util.(>>=)

let render renderer {width; height; current} r =
  let tw = 32 and th = 32 in
  ignore (Sdl.set_render_draw_color renderer 0 0xff 0 0xff);
  ignore (Sdl.render_fill_rect renderer (Some (Sdl.Rect.modify r ~x:0 ~y:0 ~w:(tw*width) ~h:(th*height))));
  for j = 0 to height-1 do
    for i = 0 to width-1 do
      let offset_to_coords direction offset =
        match direction with
        | Left -> (i*tw + (truncate (offset*.(float_of_int tw))), j*th)
        | Right -> (i*tw - (truncate (offset*.(float_of_int tw))), j*th)
        | Up -> (i*tw, j*th + (truncate (offset*.(float_of_int th))))
        | Down -> (i*tw, j*th - (truncate (offset*.(float_of_int th))))
      in

      (* This is nasty, but super-temporary. *)
      match current.(i+j*width) with
      | Floor -> ()
      | Solid _ ->
        ignore (Sdl.set_render_draw_color renderer 0xff 0 0 0xff);
        ignore (Sdl.render_fill_rect renderer (Some (Sdl.Rect.modify r ~x:(i*tw) ~y:(j*th) ~w:tw ~h:th)))
      | Beast {kind; offset; velocity} ->
        ignore (Sdl.set_render_draw_color renderer 0 255 0 255);
        ignore (Sdl.render_fill_rect renderer (Some (Sdl.Rect.modify r ~x:(i*tw) ~y:(j*th) ~w:tw ~h:th)));
        let v = kind * 25 in
        ignore (Sdl.set_render_draw_color renderer v v v 255);
        let (x,y) = offset_to_coords (match velocity with | Stationary -> Down | Moving d -> d) offset in
        ignore (Sdl.render_fill_rect renderer (Some (Sdl.Rect.modify r ~x:(x + tw/8) ~y:(y + th/8) ~w:(tw-tw/4) ~h:(th-th/4))));
        (* ignore (Sdl.render_fill_rect renderer (Some (Sdl.Rect.create ~x:(i*tw + tw/8) ~y:(j*th + th/8) ~w:(tw-tw/4) ~h:(th-th/4)))); *)

      | Player {facing; offset} ->
        ignore (Sdl.set_render_draw_color renderer 0 255 0 255);
        ignore (Sdl.render_fill_rect renderer (Some (Sdl.Rect.modify r ~x:(i*tw) ~y:(j*th) ~w:tw ~h:th)));
        ignore (Sdl.set_render_draw_color renderer 0 0 255 255);
        let (x,y) = offset_to_coords facing offset in
        ignore (Sdl.render_fill_rect renderer (Some (Sdl.Rect.modify r ~x:(x + tw/8) ~y:(y-th/4) ~w:(tw-tw/4) ~h:(th+th/8))));
    done
  done
