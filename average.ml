let rec length l = match l with
  | [] -> 0.
  | x::xs -> 1. +. length xs;;

let rec sum l = match l with
  | [] -> 0.
  | x::xs -> x +. sum xs;;

let avg l = sum l /. length l;;

avg [1.;2.;3.;4.;5.];;
