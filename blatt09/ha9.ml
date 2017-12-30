let todo _ = failwith "TODO"

let (!!) = function
| None -> failwith "'!!' failed"
| Some x -> x

let print_list l = 
  print_endline (String.concat " " (List.map (fun i -> i) l))

(* 9.4: Studenten, Studenten! *)
type student = { sname : string; age : int; semester: int }
type course = { cname : string; lecturer : string }
type grade = { student : string; course : string; grade : float }

type database = { students : student list; courses : course list; grades : grade list }

exception Database_corrupt
exception Invalid_field_type of string

let tokenize_line line = 
  List.map (fun s -> Str.global_replace (Str.regexp "\"") "" s) (Str.split (Str.regexp ",") line)

let update_students db info = 
  if (List.length info) <> 3 then raise Database_corrupt else
  let age = int_of_string_opt (List.nth info 1) in
  let semester = int_of_string_opt (List.nth info 2) in
  if age = None then raise (Invalid_field_type (List.nth info 1))
  else if semester = None then raise (Invalid_field_type (List.nth info 2))
  else
  let new_student = {sname = List.hd info; age = !!age; semester = !!semester} in
    {students = List.append db.students [new_student]; courses = db.courses; grades = db.grades}

let update_grades db info =
  if (List.length info) <> 3 then raise Database_corrupt else
  let grade = float_of_string_opt (List.nth info 2) in
  if grade = None then raise (Invalid_field_type (List.nth info 2))
  else
  let new_grade = {student = List.hd info; course = (List.nth info 1); grade = !!grade} in
    {students = db.students; courses = db.courses; grades = List.append db.grades [new_grade]}

let update_courses db info =
  if (List.length info) <> 2 then raise Database_corrupt else
  let new_course = {cname = List.hd info; lecturer = (List.nth info 1)} in
    {students = db.students; courses = List.append db.courses [new_course]; grades = db.grades}

let update_db db line =
  let line = tokenize_line line in
    match (List.hd line) with
      | "student" -> update_students db (List.tl line)
      | "grade" -> update_grades db (List.tl line)
      | "course" -> update_courses db (List.tl line)
      | _ -> raise Database_corrupt

let read_db_from_csv filename = 
  let file = open_in filename in
  let rec create_db db = 
    try create_db (update_db db (input_line file))
    with End_of_file -> db in
  create_db {students = []; courses = []; grades = []}

(* 9.5: Huffman-Kodierung *)

type charmap = (char * int) list
type huffman_tree = Node of int * huffman_tree * huffman_tree | Leaf of (char * int)
type codebook = (char * string) list

let construct_charmap input =
  let rec process_char i map =
    if i = String.length input then map else
    let c = input.[i] in
    if List.mem_assoc c map then
      let updated_list = (c, 1 + (List.assoc c map))::(List.remove_assoc c map) in
      process_char (i+1) updated_list
    else process_char (i+1) ((c, 1)::map)
  in process_char 0 []

let construct_initial_trees cm =
  let rec aux trees cm = match cm with
    | [] -> trees
    | x::xs -> aux ((Leaf x)::trees) xs
  in aux [] cm

let construct_tree cm = 
  let rec aux trees = 

  in aux (construct_initial_trees cm)

let construct_codebook = todo

let encode = todo

let decode = todo
