
open Tsdl
open Tsdl_image
let (>>=) = Util.(>>=)

type glyph =
  {
    code: int;
    x: int;
    y: int;
    width: int;
    height: int;
    x_offset: int;
    y_offset: int;
    x_advance: int;
  }

type t =
  {
    texture : Sdl.texture;
    glyphs : glyph list;        (* yuck *)
    src_rect : Sdl.rect;        (* double yuck: see HACKING.org about consing *)
    dst_rect : Sdl.rect;
  }


let load renderer path =
  let png_path = (path ^ ".png") in
  let fnt_path = (path ^ ".fnt") in
  if not Sys.(file_exists png_path && file_exists fnt_path) then
    failwith (Printf.sprintf "Couldn't find one of %s or %s" png_path fnt_path);
  let texture =
    match Image.load_texture renderer png_path with
    | None -> failwith (Printf.sprintf "Unable to load font texture %s" png_path)
    | Some t -> t
  in
  let ic = open_in fnt_path in
  (* skip the first line *)
  Printf.printf "Loading font %s\n" (input_line ic);
  let rec loop line glyphs =
    try
      let glyph =
        Scanf.sscanf line "%u %u %u %u %u %d %d %d %d"
          (fun code x y width height x_offset y_offset x_advance _y_advance ->
             {code; x; y; width; height; x_offset; y_offset; x_advance})
      in
      loop (input_line ic) (glyph :: glyphs)
    with End_of_file -> glyphs
  in
  {texture;
   glyphs = loop (input_line ic) [];
   src_rect = Sdl.Rect.create 0 0 0 0;
   dst_rect = Sdl.Rect.create 0 0 0 0}

let lookup_glyph char glyphs =
  try
    Some (List.find (fun {code} -> (Char.code char) = code) glyphs)
  with Not_found -> None

let render_line renderer {texture; glyphs; src_rect; dst_rect} (x,y) (r,g,b) string =
  Sdl.set_texture_color_mod texture r g b >>= fun () ->
  let sx,sy = ref x, ref y in
  let rec f char =
    match lookup_glyph char glyphs with
    | None -> assert (char <> '?'); f '?'
    | Some {x; y; width; height; x_offset; y_offset; x_advance;} ->
      Sdl.render_copy ~src:(Sdl.Rect.modify src_rect x y width height)
        ~dst:(Sdl.Rect.modify dst_rect (!sx + x_offset) (!sy + y_offset) width height)
        renderer texture >>= fun () ->
      sx := !sx + x_advance
  in
  String.iter f string

let free {texture} =
  Sdl.destroy_texture texture

let with_font renderer path f =
  let font = load renderer path in
  Util.unwind f font
    ~protect:(fun font -> free font)
