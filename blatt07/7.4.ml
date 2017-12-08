type const = int
type var = string
type unary_op = Neg
type binary_op = Add | Sub | Mul | Div
type expr = Const of const
          | Var of var
          | Unary of unary_op * expr
          | Binary of expr * binary_op * expr

type state = var -> const

(*a*)
let e1 = Binary (Const 3, Add, Var "x")
let e2 = Binary (Binary (Var "y", Mul, Unary (Neg, Var "x")), Add, Const 5)
let e3 = Binary(Binary(Binary(Var "x", Sub, Const 7),Mul,Var "y"),Div,Const 2)

let rec eval_expr state = function
  | Const c -> c
  | Var v -> state v
  | Unary (Neg,e) -> -eval_expr state e
  | Binary (e1,op,e2) ->
      let real_op = match op with
        | Add -> (+)
        | Sub -> (-)
        | Mul -> ( * )
        | Div -> (/) in
        real_op (eval_expr state e1) (eval_expr state e2)

let this_state = function
  | "x" -> 5
  | "y" -> -1
  | _ -> failwith "unbound"
in
  eval_expr this_state e3
