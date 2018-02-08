module Future = struct
  open Thread
  open Event
  type 'a t = 'a channel
  let get c = sync (receive c)
  let create f a = 
    let c = new_channel () in
    let rec loop f =
      f (); loop f
    in
    let task () =
      let b = f a in
      loop (fun () -> sync (send c b))
    in
    let _ = Thread.create task () in
    c (* Return value is channel *)
end

let test =
  let x = Future.create ((+) 1) 2 in
  print_int (Future.get x);
  print_int (Future.get x);
  flush stdout

let () = test