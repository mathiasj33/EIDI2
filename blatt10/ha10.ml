
module type BitGenerator = sig
  type t
  val init : int -> t
  val next : t -> (bool * t)
end

module type NumberGenerator = sig
  type t
  val init : int -> t
  val next : t -> (int * t)
end


module AlternatingBitGenerator:BitGenerator = struct
  type t = int
  let init seed = ((seed mod 2) + 1) mod 2
  let next t =
    let v = if t = 0 then 1 else 0 in
    (v=1, v)
end

module PolynomeBitGenerator:BitGenerator = struct
  type t = int
  let init seed = seed
  let next t =
    let i1 = (t lsr 3) land 1 in
    let i2 = (t lsr 17) land 1 in
    let i3 = (t lsr 24) land 1 in
    let i4 = (t lsr 29) land 1 in
    let v = i1 lxor (i2 lxor (i3 lxor i4)) in
    (v=1, (v lsl 1) lor v)
end

module MakeNumberGenerator (G:BitGenerator):NumberGenerator = struct
  type t = G.t
  let init seed = G.init seed
  let int_of_bool = function | true -> 1 | false -> 0
  let next t =
    let rec aux t number counter =
      if counter >= 31 then
        let (bit, t) = G.next t in
        ((number lor (int_of_bool) bit), t) 
      else
        let (bit, t) = G.next t in
        aux t ((number lor (int_of_bool bit)) lsl 1) (counter+1)
    in aux t 0 0
end

module RNG0 = MakeNumberGenerator (AlternatingBitGenerator)
module RNG1 = MakeNumberGenerator (PolynomeBitGenerator)

module ExtendNumberGenerator (G:NumberGenerator) = struct
  include G
  let get_int min max state =
    let rec aux t =
      let (v, t) = next t in
      if min <= v && v < max then (v, t)
      else aux t
    in aux state

  let get_float min max prec state =
    let (whole, state) = get_int min max state in
    let rec get_decimal_places t acc count =
      if count > prec then (acc, t) else
      let (next_digit, t) = get_int 0 10 t in
      let next_digit = (float_of_int next_digit)/.((float_of_int 10)**(float_of_int count)) in
      get_decimal_places t (acc +. next_digit) (count + 1)
    in get_decimal_places state (float_of_int whole) 1
end

module RNG2 = ExtendNumberGenerator (MakeNumberGenerator (PolynomeBitGenerator))

let print_bools () =
  let t = AlternatingBitGenerator.init 0b00001001000000101000000000001100 in
  let rec print_first_n t n c =
    let (v, newt) = AlternatingBitGenerator.next t in
    print_endline (string_of_bool v);
    if c >= n then () else print_first_n newt n (c+1)
  in print_first_n t 20 0

let print_numbers () =
  let t = RNG1.init 12301 in
  let rec print_first_n t n c =
    let (v, newt) = RNG1.next t in
    print_endline (string_of_int v);
    if c >= n then () else print_first_n newt n (c+1)
  in print_first_n t 20 0