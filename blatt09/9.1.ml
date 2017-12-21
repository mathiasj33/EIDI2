#load "str.cma";;

let process_line freq_map line =
  print_endline line;
  let word_list = List.map String.lowercase (Str.split (Str.regexp "[^a-zA-Z]") line) in
  let process_word z s = if (List.mem_assoc s z) then (s, 1+(List.assoc s z))::(List.remove_assoc s z) else (s, 1)::z in
    List.fold_left process_word freq_map word_list

let create_freq_map file =
  let rec update_freq_map freq_map = 
    try update_freq_map (process_line freq_map (input_line file))
    with End_of_file -> freq_map in
    update_freq_map []

let print_output file freq_map =
  List.iter
    (fun (a,b) -> Printf.fprintf file "%s=%d\n" a b) freq_map

let do_statistics i o =
  let input = open_in i in
  let freq_map = create_freq_map input in close_in input;
    let output = open_out o in
      print_output output freq_map; close_out output


;;
do_statistics "eingabe.txt" "output.txt"


