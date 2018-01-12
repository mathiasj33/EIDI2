(* uncomment following two lines to use in utop: *)
(*
#require "graphics"
#require "xml-light"
*)
open Graphics
open Xml

(* define the svg file to load here *)
let input_file = "3.svg"

let color_of_string = function
| "black" -> black
| "white" -> white
| "red" -> red
| "green" -> green
| "blue" -> blue
| "yellow" -> yellow
| "cyan" -> cyan
| "magenta" -> magenta
| _ -> black

let convert_y y = 600-y

let get_color_default e attr default =
  try
    color_of_string (attrib e attr)
  with
    Xml.No_attribute _ -> default

let isset e attr =
  try let _ = attrib e attr in true with Xml.No_attribute _ -> false

let update_color color e attr =
  set_color (get_color_default e attr color)

let circle e stroke stroke_set fill =
  let x = int_of_string (attrib e "cx") in
  let y = int_of_string (attrib e "cy") in
  let r = int_of_string (attrib e "r") in
  update_color fill e "fill";
  fill_circle x (convert_y y) r;
  if stroke_set || isset e "stroke" then (*only draw outline of stroke is set*)
    update_color stroke e "stroke";
    draw_circle x (convert_y y) r

let rect e stroke stroke_set fill =
  let x = int_of_string (attrib e "x") in
  let y = int_of_string (attrib e "y") in
  let w = int_of_string (attrib e "width") in
  let h = int_of_string (attrib e "height") in
  update_color fill e "fill";
  fill_rect x ((convert_y y) - h) w h;
  if stroke_set || isset e "stroke" then
    update_color stroke e "stroke";
    draw_rect x ((convert_y y) - h) w h

let polygon e stroke stroke_set fill =
  let points = Str.split (Str.regexp " ") (attrib e "points") in
  let points = List.map (
    fun e -> let coord_list = List.map (fun e -> int_of_string e) (Str.split (Str.regexp ",") e) in
             (List.hd coord_list, List.nth coord_list 1) 
    ) points in
  let points = List.map (fun (x,y) -> (x, convert_y y)) points in
  update_color fill e "fill";
  fill_poly (Array.of_list points);
  if stroke_set || isset e "stroke" then
    update_color stroke e "stroke";
    draw_poly (Array.of_list points)

let rec visit e stroke stroke_set fill = (* if stroke attribute is set either in a parent node or the node itsef the stroke is drawn; otherwise not*)
  match tag e with
    | "circle" -> circle e stroke stroke_set fill
    | "rect" -> rect e stroke stroke_set fill
    | "polygon" -> polygon e stroke stroke_set fill
    | "g" -> iter (fun new_e -> visit new_e 
                                (get_color_default e "stroke" stroke)
                                (stroke_set || (isset e "stroke"))
                                (get_color_default e "fill" fill)) e
    | _ -> ()

(* main function *)
let main () =
  (* open a window of size 800x600 *)
  open_graph " 800x600";
  set_line_width 2;
  let svg_tree = parse_file input_file in
  (*** extend main function here ***)
  let stroke = get_color_default svg_tree "stroke" black in
  let fill = get_color_default svg_tree "fill" black in
  iter (fun e -> visit e stroke false fill) svg_tree;

  (*********************************)
  (* wait for a key input before the app/window closes *)
  ignore (read_key ());
  close_graph ()

(* this line executes main when program is started *)
let () = main ()



