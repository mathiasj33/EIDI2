type 'a tree = Node of 'a * 'a tree * 'a tree | Leaf

let rec insert x = function
  | Node (y,l,r) ->
      if x <= y then Node (y, insert x l, r)
      else Node (y,l,insert x r)
  | Leaf -> Node (x,Leaf,Leaf)

let rec height = function
  | Node (_,l,r) -> 1 + max (height l) (height r)
  | Leaf -> 0

let (|?) a b = match a with Some x -> x | None -> b

let rec min_elem = function
  | Leaf -> None
  | Node (y,l,r) ->
      Some (min y (min (min_elem l |? y) (min_elem r |? y)))

let rec max_elem = function
  | Leaf -> None
  | Node (y,l,r) ->
      Some (max y (max (max_elem l |? y) (max_elem r |? y)))

let rec remove x = function (* matches on second argument, not on x *)
  | Leaf -> Leaf
  | Node (y,l,r) -> 
      if x=y then match l,r with 
        | Leaf, Leaf -> Leaf
        | Leaf, z -> z
        | z, Leaf -> z
        | _ -> match max_elem l with 
          | Some z -> Node (z,remove z l,r)
          | None -> failwith "impossible"
      else if x<y then Node (y,remove x l,r)
      else Node (y,l,remove x r)
