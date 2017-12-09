let rec fold_left f a = function (*end-recursive*)
  | [] -> a
  | x::xs -> fold_left f (f a x) xs

let rec fold_right f xs b =
  fold_left f b (List.rev xs)

let rec fold_right f = function (*not end-recursive*)
  | [] -> fun b -> b (*bottom, look at diagrams*)
  | x::xs -> fun b -> f x (fold_right f xs b)

let sum = fold_left (+) 0;;
let map f xs = fold_right (fun x a -> f x ::a) xs [];; (* (f x)::a *)
let map_rev f = fold_left (fun a x -> f x ::a) [];;

(* here are my own solutions (implemented myself) *)
let rec fold_left f z l = match l with
  | [] -> z
  | x::xs -> fold_left f (f z x) xs

let rec fold_right f l z = match l with
  | [] -> z
  | x::xs -> f x (fold_right f xs z)

let sum l = fold_left (+) 0 l

let map f l = fold_right (fun a b -> (f a)::b) l []

let map_rev f l = fold_left (fun a b -> (f b)::a) [] l

