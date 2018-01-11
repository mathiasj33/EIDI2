module type S = sig
  type 'a t
  val fold_left : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
  val fold_right : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val empty : 'a t
end

module Bintree : S = struct
  type 'a t = Node of 'a * 'a t * 'a t | Leaf
  let fold_left f z l = z (*TODO*)
  let fold_right f l z = z (*TODO*)
  let empty = Leaf
end

module ExtendT (X:S) = struct
  include X
  let show = X.fold_left (fun a b -> ...) "" X.t
end

module y = ExtendT (BinTree)
