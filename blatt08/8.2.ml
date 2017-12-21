type vector = float list
type matrix = vector list

let multiplys a m = List.map (fun x -> List.map (fun y -> a *. y) x) m

let multiplyv m v = 
  List.map (fun row -> List.fold_left2 (fun z b c -> z+.(b*.c)) 0. row v) m

(* let multiplyv m v = List.rev (
   List.fold_left (
   (fun acc row -> (List.fold_left2 (fun acc -> v_elem row_elem -> v_elem *. row_elem + acc) 0.0 row v)) [] m)) *)

let transpose m = 
  let m_new = List.map (fun x -> []) (List.hd m) (* Grundgerüst *)
  in
    List.fold_right
      (fun row_ori m_new -> List.fold_right2 (
                               fun row_new ele m_new -> (ele::row_new)::m_new
                             ) m_new row_ori [])
      m m_new (* m_new ist acc *)

let multiplym m1 m2 = 
  let m2 = transpose m2
  in
  let multiply_line row1 row2 = List.fold_left2 (fun acc a b -> acc+.a*.b) 0.0 row1 row2
  in
    List.fold_right (fun row1 out -> (List.fold_right (fun row2 acc -> multiply_line row1 row2) m2 [])::out) m1 []

let v = [5.;6.]
let m = [[1.;2.];[3.; 4.]]

;;
transpose [[1.;2.;3.]];;
