(* uncomment following two lines to use in utop: *)
(*
#require "graphics"
#require "xml-light"
*)

open Graphics
open Xml


(* define the svg file to load here *)
let input_file = "1.svg"

(* main function *)
let main () =
  (* open a window of size 800x600 *)
  open_graph " 800x600";
  let svg_tree = parse_file input_file in
  (*** extend main function here ***)


  (*********************************)
  (* wait for a key input before the app/window closes *)
  ignore (read_key ());
  close_graph ()

(* this line executes main when program is started *)
let () = main ()



