let argmax args f =
  let rec aux max = function
    | [] -> max
    | x::xs -> aux (if f x > f max then x else max) xs
  in aux min_float args