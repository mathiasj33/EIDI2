type 'a llist = Cons of ('a * (unit -> 'a llist))

let rec lconst x = Cons (x, fun () -> lconst x) (*funktioniert on-demand*)

let rec lseq x = Cons (x, fun () -> lseq (x+1))

let lpowers2 () = 
  let rec lpowers_from x = Cons (x, fun () -> lpowers_from (2*x)) in
    lpowers_from 1

let lfib = 
  let rec lfib_from (a:int) (b:int) = Cons (a+b, fun () -> lfib_from b (a+b)) in
    lfib_from 0 1

let lhd = function | Cons (a, f) -> a
let ltl = function | Cons (a, f) -> f ()
let rec ltake n l = if n>0 then (lhd l)::(ltake (n-1) (ltl l)) else []
let rec ldrop n l = if n>0 then ldrop (n-1) (ltl l) else l
let rec lfilter p l =
  if p (lhd l) then Cons(lhd l, fun () -> lfilter p (ltl l))
  else lfilter p (ltl l)
let rec lmap f l = Cons(f (lhd l), fun () -> lmap f (ltl l))


