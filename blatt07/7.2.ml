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
