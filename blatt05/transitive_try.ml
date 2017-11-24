let rec append l1 l2 = match l1 with
  | [] -> l2
  | x::xs -> x::(append xs l2);;

let rec transitive_one (a,b) l = match l with
  | [] -> [(a,b)]
  | (c,d)::xs -> if b = c then (a,d)::(c,d)::(transitive_one (a,b) xs) 
      else  (c,d)::(transitive_one (a,b) xs);;

let rec transitive g = match g with
  | [] -> []
  | x::xs -> append (transitive_one x xs) (transitive xs);;


append [1;2;3] [4;5;6];;

transitive_one (1,2) [(2,3); (2,4); (3,4)];;

transitive [(1,2);(2,3);(3,4)];;
