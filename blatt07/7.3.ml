type 'a tree = Node of 'a * 'a tree * 'a tree | Leaf

let rec fold_left f a = function
  | Leaf -> a
  | Node (y,l,r) -> fold_left f (f (fold_left f a l) y) r;;

let sum t = fold_left (+) 0 t;;

let to_list t = List.rev (fold_left (fun a e -> e::a) [] t);;
(*let to_list t = fold_left (fun a e -> a::e) [] t;;*)

let t = Node(2, Node(1, Leaf, Leaf), Node(3, Leaf, Leaf));;
to_list t;;
