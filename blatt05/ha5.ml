(* Ungültige Eingaben werden nicht getestet, Sie können in diesen Fällen aber folgende Exception werfen: *)
exception Invalid_input

let todo _ = failwith "TODO"


(***************
 * Aufgabe 5.4 *
 **************)
(*
int ggt(int x, int y) {
while (x != y) {
if (x > y)
x = x - y;
else
y = y - x;
}
return x;
}
*)
let rec ggt = fun x y ->
  if x=y then x
  else if x > y then ggt (x-y) y
  else ggt x (y-x);;


(***************
 * Aufgabe 5.5 *
 **************)

let rec f x y z =
  if z = 0 then x
  else if z < 0 then f (x*y*z) x (-z-1)
  else f y (x*y*z) (-z+1);;


(***************
 * Aufgabe 5.6 *
 **************)

(* Fibonacci-Zahlen beginnend mit fib 0 = 0
 * Bsp.: fib 6 = 8
*)
let rec fib n = match n with
  | 0 -> 0
  | 1 -> 1
  | _ -> fib (n-1) + fib (n-2);;

(* Summe gerader Zahlen von 0 bis n
 * Bsp.: even_sum 5 = 6
*)
let rec even_sum n = match n mod 2 with
  | 0 -> n + even_sum (n-1)
  | 1 -> even_sum (n-1)
  | _ -> 0;; (*-1 mod 2 = -1*)

(* Ist die Liste leer?
 * Bsp.: is_empty [5] = false
*)
let is_empty l = match l with
  | [] -> true
  | _ -> false;;

(* Länge einer Liste
 * Bsp.: length [6;6;6] = 3
*)
let rec length l = match l with
  | [] -> 0
  | x::xs -> 1 + length xs;;


(***************
 * Aufgabe 5.7 *
 **************)
(* Im Folgenden sollen Fehler korrigiert werden. Damit das Programm kompiliert, sind die fehlerhaften Definitionen auskommentiert und direkt darüber befindet sich eine Definiton, die zwar kompiliert, aber für die die Tests fehlschlagen. Sie können nun Schrittweise die fehlerhaften Definitionen übernehmen, also die Zeilen mit (* und *) entfernen, und versuchen zu kompilieren. Bessern Sie so alle Fehler aus. *)

(* Wir haben eine Liste von Ganzzahlen *)
let l = [7;2;31;2;96;3;25;82;3] (*ersetzen eines Kommas durch ein Semikolon*)

(*
* compute_sum berechnet die Summe der Elemente der Liste
* Beispiel:
* - Eingabe: [3;7;19;-4]
* - Ausgabe: 25
*)

let rec compute_sum l =
  match l with [] -> 0
             | x::xs -> x + compute_sum xs;; (*Einfügen eines '|'*)


(*
* double_elems fügt nach jedem Element eine Kopie des Elements in die Liste ein
* Beispiel:
* - Eingabe: [3;7;19;-4]
* - Ausgabe: [3;3;7;7;19;19;-4;-4]
*)

let rec double_elems =
  function [] -> []
         | x::xs -> x :: x :: double_elems xs;; (*Entfernen des Parameters auf der linken Seite des '='*)

(*
* count_greater zählt wieviele Elemente der Liste größer als der gegebene Wert sind
* Beispiel:
* - Eingabe: 3 [3;7;19;-4]
* - Ausgabe: 2
*)
let count_greater = todo
(*
let count_greater v l =
let rec impl c =
function [] -> d
| x::xs -> if x > v then impl c+1 xs else impl c xs
in
impl 0 l
*)

(*
* gen_seq erzeugt eine Liste von aufsteigenden Zahlen
* Beispiel:
* - Eingabe: 6
* - Ausgabe: [0;1;2;3;4;5;6]
*)
let rec gen_seq = todo
(*
let rec gen_seq n =
let rec impl k
if k = n then k else k :: impl (k+1)
in
impl 0
*)

(* add_half addiert 0.5 zu allen Werten der Liste (diese müssen dazu in float umgewandelt werden)
 * Beispiel:
 * - Eingabe: [3;7;19;-4]
 * - Ausgabe: [3.5;7.5;19.5;-3.5]
*)
let add_half = todo
(*
let add_half l =
| [] -> []
| x::xs ->
let x::xs = l in
((float_of_int x) + 0.5) :: add_half xs
*)
