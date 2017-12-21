let todo _ = failwith "todo"

type node = int
type graph = (node * float * node) list

type coloring = (node * int) list
type color = int

type stmt = Assign of string * int

(* Test examples *)
let g = [
  (1,3.0,2);
  (1,10.0,3);
  (2,8.0,5);
  (2,4.0,4);
  (3,1.0,4);
  (3,5.0,6);
  (4,7.0,6);
  (4,12.0,5);
  (5,2.0,6);
  (7,1.5,1)
]

let incorrect_coloring = [
  (1,0);
  (2,1);
  (3,2);
  (4,2);
  (5,0);
  (6,2);
  (7,0);
]

let correct_coloring = [
  (1,0);
  (2,1);
  (3,2);
  (4,0);
  (5,2);
  (6,1);
  (7,1)
]


(* 8.4 - 1: List.fold_left fl1 0 l
 *
 * Beispiel: List.fold_left fl1 0 [[37;13];[];[1032];[139;9;1;2;4];[391;11]]
 * Ergebnis: 10
*)
let fl1 z l = z + List.length l

(* 8.4 - 2: List.fold_left fl2 (max_int,min_int) l
 *
 * Beispiel: List.fold_left fl2 (max_int,min_int) [1932;324;43;134;2865;9582;85]
 * Ergebnis: (43,9582)
*)
let fl2 z v = match z with
  | (min, max) -> if v < min then
        if v > max then (v, v)
        else (v, max)
      else if v > max then (min, v)
      else (min, max)

(* 8.4 - 3: List.fold_left fl3 [] l
 *
 * Beispiel: List.fold_left fl3 [] ['g';'z';'x';'a']
 * Ergebnis: ['a';'x';'z';'g';'g';'z';'x';'a']
*)
let fl3 z v = v::(List.append z [v])

(* 8.4 - 4: List.fold_left fl4 (fun _ -> 0) l
 *
 * Beispiel: List.fold_left fl4 (fun _ -> 0) [Assign ("a", 3); Assign ("b", 9); Assign ("a", 7)]
 * Ergebnis: Eine Funktion f, sodass (f "a" = 7) und (f "b" = 9)
*)
let fl4 z stmt = match stmt with Assign (s, v) -> (fun x -> if x = s then v else (z x))

(* 8.4 - 5: List.fold_left fl5 [] l
 *
 * Beispiel: List.fold_left fl5 [] [Assign ("a", 3); Assign ("b", 9); Assign ("a", 7)]
 * Ergebnis: [("a",7);("b",9)]
*)
let fl5 z stmt = match stmt with Assign (s, v) ->
  if List.mem_assoc s z then (s, v)::(List.remove_assoc s z)
  else (s, v)::z

(* 8.4 - 6: List.fold_left fl6 [] l
 *
 * Beispiel: List.fold_left fl6 [] [(fun x -> x + 1); (fun x -> x - 13); (fun x -> (x + 2) * 7)]
 * Ergebnis: [1;-13;14]
*)
let fl6 z f = List.append z ([f 0])



(* 8.5 - 1 *)
let nodes graph = List.fold_left 
                    (fun acc (n1, w, n2) -> 
                       if List.exists (fun e -> e = n1) acc then
                         if List.exists (fun e -> e = n2) acc then
                           acc
                         else n2::acc
                       else if List.exists (fun e -> e = n2) acc then
                         n1::acc
                       else n1::n2::acc)
                    [] graph

(* 8.5 - 2 
   Beispiel: 4 [(0, 0.5, 1); (1, 0.2, 3); (1, 0.8, 4); (2, 0.4, 4)] ->
   [(2, 0.4); (1; 0.8)]
*)
let adjacent_nodes node graph = List.fold_left
                                  (fun acc (n1, w, n2) ->
                                     if n1 = node then (n2, w)::acc
                                     else if n2=node then (n1, w)::acc
                                     else acc)
                                  [] graph

(* 8.5 - 3 *)
let init_dists start graph = List.fold_left (fun acc n -> if n=start then (n, 0.)::acc else (n, infinity)::acc) [] (nodes graph)

(* 8.5 - 4 *)
let sort_nodes_by_dists nodes dists =
  List.sort (fun n1 n2 -> 
              let comp = (List.assoc n1 dists) -. (List.assoc n2 dists) in
                if comp < 0. then -1 else if comp > 0. then 1 else 0) nodes

(* 8.5 - 5 *)
let dijkstra start graph =
  let rec process acc open_nodes closed_nodes dists =
    match open_nodes with
      | [] -> acc
      | xs -> 
          let open_nodes = sort_nodes_by_dists open_nodes dists in
            (match open_nodes with
              | [] -> failwith "impossible"
              | x::xs -> 
                  (let dist = List.assoc x dists in
                   let all_adjs = adjacent_nodes x graph in
                   let adjs = 
                     List.filter 
                       (fun (n, ndist) -> 
                          not (List.mem_assoc n closed_nodes)) all_adjs in
                   let closed_nodes = (x,true)::closed_nodes in
                   let new_dists = 
                     List.fold_left (
                       fun z (n, d) ->
                         if List.exists (fun (e, ed) -> e=n) adjs then
                           if dist+.(List.assoc n adjs) < d then
                             (n, dist+.(List.assoc n adjs))::z
                           else (n, d)::z
                         else (n ,d)::z
                     ) [] dists in
                   let new_open_nodes =
                     let adj_nodes = (List.map (fun (n, d) -> n) adjs) in
                       List.append adj_nodes
                         (List.filter 
                            (fun n -> (not (List.exists (fun n2 -> n2=n) adj_nodes)))
                            xs) in
                     process ((x, dist)::acc) new_open_nodes closed_nodes new_dists
                  ))
  in process [] [start] [] (init_dists start graph)

(* 8.6 - 1 *)
let rec check_coloring c graph = 
  let rec aux = function
    | [] -> true
    | x::xs ->
        if not (List.mem_assoc x c) then false
        else
        if not 
             (List.for_all (fun n -> (List.mem_assoc n c) && ((List.assoc n c)<>(List.assoc x c)))
                (List.map (fun (n,w) -> n) (adjacent_nodes x graph))) then false
        else aux xs
  in
    aux (nodes graph)


let sort_nodes_by_degree nodes degrees =
  List.sort (fun n1 n2 -> 
              let comp = (List.assoc n1 degrees) - (List.assoc n2 degrees) in
                if comp < 0 then 1 else if comp > 0 then -1 else 0) nodes

let degrees graph = 
  let rec aux = function
    | [] -> []
    | x::xs ->
        (x, List.length (adjacent_nodes x graph))::(aux xs)
  in
    aux (nodes graph)

let choose_first_not_in color_list =
  let rec aux i =
    if (List.exists (fun e -> e=i) color_list) then aux (i+1) else i
  in aux 0

let print_list l = 
  print_endline (String.concat " " (List.map string_of_int l))

(* 8.6 - 2 *)
let do_coloring graph =
  let rec aux (c:coloring) = function
    | [] -> c
    | x::xs ->
        let neighbor_colors =
          let neighbor_nodes = List.map (fun (n,w) -> n) (adjacent_nodes x graph) in
            List.filter (fun e -> e >= 0) (List.map (fun n -> if List.mem_assoc n c then List.assoc n c else -1) neighbor_nodes) in
        let color = choose_first_not_in neighbor_colors in
          aux ((x,color)::c) xs
  in
    aux [] (sort_nodes_by_degree (nodes graph) (degrees graph))

;;
do_coloring [(1,2.40,2)]
