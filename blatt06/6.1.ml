module Pair = struct
  let flip (a,b) = b,a
end

let () = 
  let a = 1, "one" in  (*a is a tuple*)
  let b = 1, "one" in
    print_endline (string_of_bool (a==b));
    print_endline (string_of_bool (a=b));
    print_endline (fst (Pair.flip a));


