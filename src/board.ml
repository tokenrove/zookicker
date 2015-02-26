type direction = Up | Down | Left | Right

type beast =
  {
    kind: int;
    position: float*float;      (* relative to current tile *)
    velocity: float*float;      (* in tiles/millisecond *)
  }

type player =
  {
    facing: direction;
    position: float*float;      (* relative to current tile *)
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
    mutable spare : board;
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
      | '@' -> Player {facing=Down; position=(0.,0.)}
      | '1'..'9' as c -> Beast {kind=(Char.code c)-(Char.code '0');
                                position=(0.,0.);
                                velocity=(0.,0.)}
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
        Some {width; height; player_pos; current = board; spare = Array.copy board}
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

let update_tile dt p present =
  let sink v =
    copysign (max (abs_float(v) -. dt) 0.) v in
  let tile = present.(p) in
  match tile with
  | Player {facing; position=(x,y)} ->
    (* to update the player, we ease their position towards (0,0) *)
    Player {facing; position=(sink x, sink y)}
  | Beast _ -> tile
  | Solid _ | Floor -> tile

let update dt board =
  let {width; height; current; spare} = board in
  let changed = ref false in
  for j = 0 to height-1 do
    for i = 0 to width-1 do
      let p = i+j*width in
      let t = update_tile dt p current in
      if spare.(p) <> t then
        changed := true;
      spare.(p) <- t
    done
  done;
  board.current <- spare;
  board.spare <- current;
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

let unit_delta_of_direction = function
  | Left -> (-1,0)
  | Right -> (1,0)
  | Up -> (0,-1)
  | Down -> (0,1)

let get_adjacent_position {width; height;} direction (x,y) =
  assert (x >= 0 && x < width && y >= 0 && y < height);
  let (dx,dy) = unit_delta_of_direction direction in
  (width+x+dx) mod width, (height+y+dy) mod height

let move direction ({player_pos=(x,y); width; current} as board) =
  let tile = current.(x+y*width) in
  let (x',y') = get_adjacent_position board direction (x,y) in
  let char_of_tile = function
    | Floor -> '.'
    | Solid _ -> '*'
    | Player _ -> '@'
    | Beast {kind} -> Char.chr ((Char.code '0')+kind)
  in
  match tile with
  | Player {facing; position=(dx,dy)} ->
    if abs_float(dx) < epsilon_float &&
       abs_float(dy) < epsilon_float &&
       current.(x'+y'*width) = Floor then begin
      let (dx',dy') = unit_delta_of_direction direction in
      current.(x'+y'*width) <- Player {facing=direction;
                                       position=(float_of_int dx', float_of_int dy');};
      current.(x+y*width) <- Floor;
      board.player_pos <- (x',y')
    end
  | _ -> failwith "I was told there would be a player at this position"

let kick board = ()
