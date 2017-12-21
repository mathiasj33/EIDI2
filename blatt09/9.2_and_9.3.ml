let f x = x+5

;;

(* @@: Anwenden der Funktion auf 'a -> Klammerung weglassen *)
f (f 5) (* entspricht f @@ f 5 *)
(* f(g(h x)) = f @@ g @@ h x *)
;;
(* |>: Wie @@, vertauschte Parameter -> Wie Unix Pipe *)
(* f(g(h x)) = h x |> g |> f *)
;;
let g = fun x -> x+1;;
let f = fun x -> x mod 2 = 0;;
List.map g (List.filter f [1;2;3;4;5]);;
List.map g @@ List.filter f [1;2;3;4;5];;
List.filter f [1;2;3;4;5] |> List.map g;;

(* %, %>: Komposition mit unterschiedlicher Reihenfolge *)

let k f1 f2 c = f1 (f2 c) (* entspricht (%) *);;
((List.map g) % (List.filter f)) [1;2;3;4;5];;

(*9.3*)

let f1 x = x;; (*einzige Möglichkeit, da 'a generisch*)
let f2 x y = x;; (*einzige*)
let f3 f b a = f a b (*entspricht flip der params; einzige*)
let f4 f a b = f (a, b) (*ent-tupeln; einzige *)
let f5 f g c = f (g c) (* wie (%)-operator *)
let f6 stmt a = stmt a; a (*führt Seiteneffekt aus und gibt a zurück *)
let f7 (a:unit) (b:unit):unit = ()
let f7' () () = ();;
(*8. true oder false *) let f8 = [true; false] (*mögl. 1: true; mögl. 2: false*);;

let f9 = [id; not; f2 true; f2 false] (*mögl. 1: id, mögl. 2: not, ...*)

