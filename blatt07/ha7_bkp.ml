let todo _ = failwith "todo"

(* Typdefinitionen: Ausdrücke *)
type const = int
type var = string
type unary_op = Neg
type binary_op = Add | Sub | Mul | Div
type expr = Const of const 
          | Var of var 
          | Unary of unary_op * expr 
          | Binary of expr * binary_op * expr

(* Typdefinitionen: Programmzustand *)
type state = var -> const

(* Typdefinitionen: Bedingungen *)
type comp_op = Eq | Neq | Le | Leq
type cond = True | False | Comp of expr * comp_op * expr 
          | Not of cond | And of cond * cond | Or of cond * cond
type stmt = Assign of var * expr | Print of var | IfThenElse of cond * stmt list * stmt list | While of cond * stmt list
type prog = stmt list

(* Hilfsfunktionen: string_of_* *)
let rec string_of_expr = 
  let string_of = function Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/"
  in function
    | Const c -> string_of_int c
    | Var v -> v 
    | Unary (Neg, e) -> "-" ^ (string_of_expr e)
    | Binary (e1, op, e2) -> "(" ^ (string_of_expr e1) ^ (string_of op) ^ (string_of_expr e2) ^ ")"

let rec string_of_cond = 
  let string_of = function Eq -> "==" | Neq -> "!=" | Le -> "<" | Leq -> "<=" 
  in function
    | True -> "true"
    | False -> "false"
    | Not c -> "!" ^ (string_of_cond c)
    | Comp (e1, op, e2) -> (string_of_expr e1) ^ (string_of op) ^ (string_of_expr e2)
    | And (c1, c2) -> "(" ^ (string_of_cond c1) ^ " && " ^ (string_of_cond c2) ^ ")"
    | Or (c1, c2) -> "(" ^ (string_of_cond c1) ^ " || " ^ (string_of_cond c2) ^ ")"

let rec string_of_state vars sta =
  "{ " ^ (String.concat ", " (List.map (fun v -> v ^ "=" ^ (string_of_int (sta v))) vars)) ^ " }"

(* Hinweis: ?(nobreak=false) ist ein optionaler Parameter und kann ignoriert werden.
   Die Funktion kann einfach mit einem Argument vom Typ stmt aufgerufen werden *)
let rec string_of_stmt ?(nobreak=false) s =
  let br = if nobreak then "" else "\n" in 
  let rec impl i = 
    let ind = String.make (i*2) ' ' in 
      function
        | Assign (v, e) -> ind ^ v ^ " = " ^ (string_of_expr e) ^ ";"
        | Print v -> ind ^ "print(" ^ v ^ ");"
        | IfThenElse (c, t, e) -> 
            let ts = String.concat br (List.map (impl (i+1)) t) in
            let es = String.concat br (List.map (impl (i+1)) e) in
              ind ^ "if(" ^ (string_of_cond c) ^ ") {" ^ br 
              ^ ts ^ br ^ ind ^ "} else {" ^ br ^ es ^ br ^ ind ^ "}"
        | While (c, b) -> 
            let bs = String.concat br (List.map (impl (i+1)) b) in
              ind ^ "while(" ^ (string_of_cond c) ^ ") {" ^ br ^ bs ^ br 
              ^ ind ^ "}"
  in 
    impl 0 s

let string_of_prog p = 
  (String.concat "\n" (List.map string_of_stmt p)) ^ "\n"


(* Ein Beispielprogramm, welches die Fakultät der Variablen 'arg' berechnet und
   das Ergebnis in der Variablen 'ret' speichert. Einzelne Zwischenschritte werden
   mit der Print-Anweisung ausgegeben. 

   Sie können dieses Programm nutzen um Ihre eval_prog Funktion zu testen. 

   Glücklicherweise enthält das Programm auch einige überflüssige und ineffiziente
   Anweisungen und Berechnungen.

   Sie können dieses Programm deshalb auch nutzen um Ihre opt Funktion zu testen.
*)
let bad_fac = [
  Assign ("ret", Const 1);
  Assign ("z", Binary (Var "arg", Sub, Const 1));
  Assign ("b", Const 4);
  Assign ("x", Binary (Binary (Const 12, Sub, Const 11), Mul, Var "arg"));
  While (And (Comp (Const 0, Le, Var "x"), Or (True, Comp (Var "z", Leq, Const 1))), [
           Assign ("a", Binary (Const 3, Mul, Binary (Const 5, Add, Const 10)));
           IfThenElse (Comp (Binary (Binary (Const 0, Mul, Var "x"), Add, Binary (Var "arg", Sub, Var "arg")), Le, Const 1), [
                         Assign ("y", Binary (Var "a", Sub, Const 40));
                       ], [
                         Assign ("y", Binary (Const 40, Sub, Var "a"));
                         Assign ("b", Binary (Var "z", Add, Const 1));
                         Assign ("z", Binary (Var "b", Mul, Const 2));
                       ]);
           While (Comp (Const 8, Neq, Binary (Const 9, Sub, Binary (Var "b", Div, Var "b"))), [
                    Assign ("z", Binary (Var "z", Add, Const 2));
                  ]);
           Assign ("ret", Binary (Var "x", Mul, Binary (Var "ret", Mul, Var "y")));
           Assign ("x", Binary (Var "x", Sub, Const 1));
           Assign ("ret", Binary (Var "ret", Div, Const 5));
           Print ("ret");
         ]);
]

(* 7.4 - 2 *)
let rec eval_expr state = function
  | Const c -> c
  | Var v -> state v
  | Unary (Neg, e) -> - eval_expr state e
  | Binary (e1, op, e2) -> 
      (match op with Add -> (+) | Sub -> (-) | Mul -> ( * ) | Div -> (/))
        (eval_expr state e1) (eval_expr state e2)

let rec eval_cond state = function
  | True -> true
  | False -> false
  | Comp (e1, op, e2) ->
      (match op with Eq -> (=) | Neq -> (<>) | Le -> (<) | Leq -> (<=))
        (eval_expr state e1) (eval_expr state e2)
  | Not c -> not (eval_cond state c)
  | And (c1, c2) -> (eval_cond state c1) && (eval_cond state c2)
  | Or (c1, c2) -> (eval_cond state c1) || (eval_cond state c2)

let rec eval_stmt_list state = function
  | [] -> state
  | x::xs -> eval_stmt_list (eval_stmt state x) xs
and
  eval_stmt state = function
  | Assign (v,e) -> (fun v2 -> if v2 = v then eval_expr state e else state v2)
  | Print v -> print_int (state v); print_newline (); state
  | IfThenElse (cond, l1, l2) ->
      if eval_cond state cond then
        eval_stmt_list state l1
      else 
        eval_stmt_list state l2
  | While (cond, l) -> eval_while state cond l
and
  eval_while state cond l = 
  if not (eval_cond state cond) then state else
    let new_state = eval_stmt_list state l in
      eval_while new_state cond l

let eval_prog c p = 
  (eval_stmt_list (fun x -> if x = "arg" then c else 0) p) "ret"

(* Aufgabe 7.5: Meine eigene Programmiersprache *)
(* 7.5 - 1 *)
let prog1 = [
  Assign ("x", Const 4);
  IfThenElse (Or (Comp (Var "arg", Le, Const 0), Comp (Var "z", Neq, Const 0)),
              [
                Assign ("y", Unary (Neg, Var "arg"));
                Assign ("x", Binary (Var "x", Mul, Const 3));
              ],
              [
                Assign ("y", Binary (Const 3, Mul, Var "arg"))
              ]);
  Assign ("z", Binary (Binary (Var "x", Mul, Var "y"), Add, Const 10));
  Print ("z");
  Assign ("ret", Var "z")
]


(* Aufgabe 7.6: Jetzt wird's noch besser! *)
(* 7.6 - 1 *)
let rec match_not_equal left op right =
  let real_op = 
    (match op with Add -> (+) | Sub -> (-) | Mul -> ( * ) | Div -> (/)) in
    match left with
      | Const x -> 
          begin match x with
            | 0 -> 
                begin match op with
                  | Add -> right
                  | Sub -> pe_expr_opt (Unary (Neg, right))
                  | Mul -> Const 0
                  | Div -> Const 0
                end
            | 1 -> 
                begin match op with
                  | Mul -> right
                  | _ -> 
                      begin 
                        match right with
                          | Const y -> Const (1+y)
                          | _ -> Binary (left, op, right)
                      end
                end
            | _ ->
                begin match right with
                  | Const y -> Const (real_op x y)
                  | _ -> Binary (left, op, right)
                end
          end
      | _ ->
          begin match right with
            | Const y -> 
                begin match y with
                  | 0 -> 
                      begin match op with
                        | Add -> left
                        | Sub -> left
                        | Mul -> Const 0
                        | Div -> Const 1 (* laut Tests... *)
                      end
                  | 1 -> 
                      begin match op with
                        | Mul -> left
                        | Div -> left
                        | _ -> Binary (left, op, right)
                      end
                  | _ -> Binary (left, op, right)
                end
            | _ -> Binary (left, op, right)
          end
and 
  pe_expr_opt = function
  | Const x -> Const x
  | Var x -> Var x
  | Unary (Neg, x) -> 
      begin match x with
        | Const a -> Const (-a)
        | _ -> Unary (Neg, x)
      end
  | Binary (e1, op, e2) ->
      let left = pe_expr_opt e1 in
      let right = pe_expr_opt e2 in
        if left = right then
          match op with
            | Sub -> Const 0
            | Div -> Const 1
            | _ -> match_not_equal left op right
        else match_not_equal left op right

;;

(*
;;
pe_expr_opt (Binary(Var "x", Mul, Binary (Var "t", Sub, Var "t")));;
pe_expr_opt (Binary (Const 0, Mul, Var "x"))*)


let pe_cond_opt = todo
let pe_opt = todo

(* 7.6 - 2 *)
let dce_opt = todo

(* 7.6 - 3 *)
(*let test_prog = [
  Assign ("x", Binary (Const 5, Add, Const 10));
  Assign ("y", Binary (Const 5, Add, Const 0));
  Assign ("y", Binary (Var "x", Mul, Const 0));
  Assign ("y", Binary (Var "x", Sub, Var "x"));
  ] *)
let opt = todo


