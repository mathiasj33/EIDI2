module type Map = sig (* t kann zB Liste sein *)
  type ('a, 'b) t
  val empty : ('a, 'b) t
  val set : 'a -> 'b -> ('a, 'b) t -> ('a, 'b) t
  val get : 'a -> ('a, 'b) t -> 'b option
  val remove : 'a -> ('a, 'b) t -> ('a, 'b) t
  val keys : ('a, 'b) t -> 'a list
end

module ExtendMap (M:Map) = struct
  include M
  let pairs m = List.map (fun x -> (x, (match M.get x m with Some a -> a | None -> failwith "impossible"))) (M.keys m)
  let from_pairs l = List.fold_left (fun z (k,v) -> M.set k v z) (M.empty) l
  let values m =
    let p = pairs m in
    List.map snd p
  let map_values f m = List.fold_left (fun z (k,v) -> M.set k (f v) z) M.empty (pairs m)
  (* let filter_values f m = List.fold_left (fun z (k,v) -> if f v then z else M.remove k z) m (pairs m) *)
  let filter_values f m = from_pairs (List.filter (fun (k,v) -> f v) (pairs m))
end

module ExtendedListMap = ExtendMap (ListMap)
module ExtendedTreeMap = ExtendMap (TreeMap)

(*module TreeMap:Map = struct
  type ('a, 'b) t = 'a * 'b * ('a, 'b) t * ('a, 'b) t | Leaf
  ...
end

module ListMap:Map = struct
  type ('a, 'b) t = ('a * 'b) list
  let empty = []
  let set a b map = map
  let get a map = List.mem_assoc_opt a map
  let remove a map = map
  let keys a map = [a]
end *)