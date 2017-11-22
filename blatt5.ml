let rec ggt x y = 
  if x=y then x
  else if x > y then ggt (x-y) y
  else ggt x (y-x);;

let rec f x y z =
  if z = 0 then x
  else if z < 0 then f (x*y*z) x (-z-1)
  else f y (x*y*z) (-z+1);;

let rec fib n = match n with
  | 0 -> 0
  | 1 -> 1
  | _ -> fib (n-1) + fib (n-2);;

let rec even_sum n = match n mod 2 with
  | 0 -> n + even_sum (n-1)
  | 1 -> even_sum (n-1)
  | _ -> 0;;

let is_empty l = match l with
  | [] -> true
  | _ -> false;;

let rec length l = match l with
  | [] -> 0
  | x::xs -> 1 + length xs;;

(*Komma entfernen
  | fehlt
  function durch fun ersetzen
*)
