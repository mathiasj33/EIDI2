module Parallel = struct
  open Event
  let comp2 f g x y = f (g x) (g y)
  let compareBy f = comp2 compare f (*compare Ocaml intern; -1, 0 oder 1 *)
  let map f l = 
    let t f x = (*main thread*)
      let c = new_channel () in
      let _ = Thread.create (fun x -> sync (send c (f x))) x in
      receive c (*blockiert nicht; Liefert event*)
    in
    (*for sorting*)let fs = List.mapi (fun i x -> Event.wrap (t f x) (fun y -> i, y)) l in (*i: counter; wrap e f: f e, sobald e fertig ist- Rückgabe: Event *)
    let ys = List.map (fun _ -> Event.select fs) fs in (* ys is List of tuples *)
    (*for sorting*)List.map (snd) (List.sort (compareBy fst) ys)
end