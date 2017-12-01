let todo _ = failwith "TODO"

module MyList = struct
  (* length of an 'a list *)
  let rec length = function
    | [] -> 0
    | x::xs -> 1 + length xs

  (* average of an int list as float *)
  let rec avg l = 
    let rec aux len = function
      | [] -> 0.
      | x::xs -> (float_of_int x) /. len +. aux len xs
    in 
      match l with
        | [] -> None
        | xs -> Some (aux (float_of_int (length xs)) xs)

  (* and for float list: *)
  let rec sumf = function
    | [] -> 0.
    | x::xs -> x +. sumf xs

  let avgf l = 
    let rec aux len = function
      | [] -> 0.
      | x::xs -> x /. len +. aux len xs
    in 
      match l with
        | [] -> None
        | xs -> Some (aux (float_of_int (length xs)) xs)

  (* list concatenation, same as @ (will be undefined in the test environment!) *)
  let append a = todo
  let (@) = append (* infix operator for append *)
  (* palindrome ['a';'b'] = ['a';'b';'b';'a'] *)
  let palindrome = todo

  (* note the expected types in ha6.mli! *)
(*
* #show_type option;;
* type nonrec 'a option = None | Some of 'a
*)
  (* optional first element. head [] = None, head [1;2] = Some 1 *)
  let head = todo
  (* optional rest, after first element. tail [] = tail [1] = None, tail [1;2] = Some [2] *)
  let tail = todo
  (* optional last element. last [] = None, last [1;2] = Some 2 *)
  let last = todo

  (* reverse a list *)
  let reverse = todo
  (* check whether argument is a palindrome *)
  let is_palindrome = todo
  (* multiply each element with 2 *)
  let times2 = todo
  (* result should only contain even numbers *)
  let even = todo
  (* at least one element must be true *)
  let one = todo
  (* all elements must be true. all [] = true. *)
  let all = todo
end

let (|?) a b = match a with Some x -> x | None -> b

let () = 
  print_endline (string_of_float (MyList.avgf [1.;2.3;4.21] |? 0.));
  (* print_endline (string_of_float (MyList.sumf [1.;2.3;4.2])) *)
(*
module NonEmptyList = struct
type 'a t = Cons of 'a * 'a t | Nil of 'a

let from_list = todo
let to_list = todo

let head = todo
let tail = todo
let last = todo
end


module Db = struct
(* we assume that name is unique here *)
type student = { sname : string; age : int; semester: int }
type course = { cname : string; lecturer : string }
type grade = { student : string; course : string; grade : float }

let students = [
{ sname = "Student 1"; age = 19; semester = 1 };
{ sname = "Student 2"; age = 24; semester = 7 };
{ sname = "Student 3"; age = 28; semester = 12 };
{ sname = "Student 4"; age = 23; semester = 4 };
]
let courses = [
{ cname = "Course 1"; lecturer = "Prof. 1" };
{ cname = "Course 2"; lecturer = "Prof. 2" };
{ cname = "Course 3"; lecturer = "Prof. 1" };
]
let grades = [
{ student = "Student 1"; course = "Course 1"; grade = 2.7 };
{ student = "Student 1"; course = "Course 2"; grade = 1.0 };
{ student = "Student 2"; course = "Course 1"; grade = 4.0 };
{ student = "Student 2"; course = "Course 2"; grade = 5.0 };
{ student = "Student 3"; course = "Course 3"; grade = 3.7 };
]

(* find a student by name *)
let find_student = todo

(* all averages are of type float option *)
(* calculate the average age of students that are in a given semester or above *)
let avg_age = todo
(* calculate the grade average of a student *)
let avg_grade_student = todo
(* calculate the grade average of a course *)
let avg_grade_course = todo
(* calculate the grade average of a course for students in a given semester *)
let avg_grade_course_semester = todo
end


module KdTree = struct
type point = float list
type kdtree = Empty | Leaf of point | Node of float * kdtree * kdtree

(* Generiert eine Liste von n k-dimensionalen Punkten *)
let gen_random_points (k : int) (n : int) : point list =
let rec random_point k acc =
if k <= 0 then acc else
let r = 50.0 -. Random.float 100.0 in
random_point (k-1) (((float_of_int (int_of_float (r *. 100.0))) /. 100.0) :: acc)
in
let rec impl n acc =
if n <= 0 then acc else impl (n-1) ((random_point k []) :: acc)
in
impl n []

(* Erzeugt eine string-Repräsentation eines Punktes *)
let string_of_point (p : point) =
"(" ^ String.concat "," (List.map string_of_float p) ^ ")"

(* Schreibt einen kdtree im dot-Format in die Datei fname *)
let print_dot (fname : string) (tree : kdtree) : unit =
let rec get_d = function
| Empty -> 0
| Leaf p -> List.length p
| Node (_, l, r) -> max (get_d l) (get_d r)
in
let d = get_d tree in
let out = open_out fname in
let rec write_node x i t : (int * int option) =
match t with
| Empty -> i, None
| Leaf p ->
Printf.fprintf out "n%d[shape=rectangle,label=\"%s\"];\n" i (string_of_point p);
i+1, Some i
| Node (p, l, r) ->
let x = 1 + ((x-1) mod d) in
Printf.fprintf out "n%d[label=\"x%d = %.2f\"];\n" i x p;
let (i', lid_opt) = write_node (x+1) (i+1) l in
let (i', rid_opt) = write_node (x+1) i' r in
(match lid_opt with None -> ()
| Some lid -> Printf.fprintf out "n%d -> n%d[label=\"l\"];\n" i lid);
(match rid_opt with None -> ()
| Some rid -> Printf.fprintf out "n%d -> n%d[label=\"r\"];\n" i rid);
i', Some i
in
Printf.fprintf out "digraph {\n";
ignore(write_node 1 0 tree);
Printf.fprintf out "}";
close_out out


(* kd_create : point list -> kdtree *)
let kd_create = todo

(* kd_points : kdtree -> point list *)
let kd_points = todo

type aabb = point * point

(* kd_points_in_aabb : aabb -> kdtree -> point list *)
let kd_points_in_aabb = todo
end
*)
