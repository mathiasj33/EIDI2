let rec append l e = match l with
  | [] -> [e]
  | x::xs -> x::(append xs e);;

let rec reverse l = match l with
  | [] -> []
  | [x] -> l
  |x::xs -> append (reverse xs) x;;

reverse [1;2;3;4;5]
