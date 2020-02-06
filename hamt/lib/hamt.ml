(* TODO: functor modules *)
(* TODO: bitmap optimization *)
(* TODO: tail optimization *)
(* TODO: transient optimization *)

type key = string

type 'a t = {
  mutable length: int;
  internal: 'a internal
}
and 'a internal =
  | Empty
  | Leaf of key * 'a
  | Node of 'a internal array   (* make sure it's structural share *)

let array_len = 32

(* TODO: remove int restriction *)
let empty : int t = {
  length = 0;
  internal = Empty
}

let leaf k v = Leaf (k, v)

let node children = Node children

let extract_children = function
  | Node children -> children
  | _ -> [||]

let hash = Base.String.hash

let segments n base =
  let safe_wrap x =
    if x = -1 then base - 1 else x in
  let rec loop n acc =
    if n < base then
      (safe_wrap (n - 1)) :: acc
    else
      let acc = (safe_wrap (n mod base - 1)) :: acc in
      loop (n / base) acc in
  loop n []

exception Something_wrong

let is_node = function
  | Node (_) -> true
  | _ -> false

let is_empty = function
  | Empty -> true
  | _ -> false

let insert children segments k v =
  let rec loop children = function
    | idx :: [] ->
      begin
        match Array.get children idx with
        | Empty ->
          Array.set children idx (leaf k v)
        | _ ->
          raise Something_wrong
      end
    | idx :: tl ->
      if (is_empty children.(idx)) then
        Array.set children idx (node (Array.make array_len Empty));
      loop (extract_children children.(idx)) tl
    | [] ->
      ()
  in
  loop children segments

(* path copy *)
let assoc k v {length; internal} =
  match internal with
  | Empty ->
    {
      length = 1;
      internal = leaf k v
    }
  | Leaf (k2, v2) ->
    if k2 = k then
      {
        length = 1;
        internal = leaf k v
      }
    else
      let children = Array.make array_len Empty in
      let segments1 = segments (hash k) array_len in
      let segments2 = segments (hash k2) array_len in
      let _ = insert children segments2 k2 v2 in
      let _ = insert children segments1 k v in
      {
        length = 2;
        internal = Node children
      }
  | Node children ->
    {
      length;
      internal = Node children
    }

let print_children children =
  let s = match children with
    | Empty -> "Empty "
    | Leaf (_a, _b) -> "Leaf "
    | Node _ -> "Node " in
  Printf.printf "%s " s

let find k {internal; _} =
  let segments = segments (hash k) array_len in
  let rec loop children l =
    match l with
    | [] ->
      begin match children with
        | Leaf (k', v) ->
          if k = k' then Some v else None
        | _ ->
          None
      end
    | (current :: next) ->
      let _ = Printf.printf "current index: %d.\n" current in
      let _ = print_children children in
      let _ = print_newline () in
      match children with
      | Empty | Leaf _ -> None
      | Node children ->
        loop children.(current) next in
  loop internal segments

(* create pointers to all children for the current array node except the
   child which is in the finding path.
*)
let preserve_pointers children path_idx children' new_leaf =
  let children = extract_children children in
  let children' = extract_children children' in
  let idx_children = ref Empty in
  for i = 0 to Array.length children - 1 do
    let child = if i = path_idx then
        match children.(i) with
        | Leaf _ ->
          new_leaf
        | _ ->
          let _ = idx_children := node (Array.make array_len Empty) in
          !idx_children
      else
        children.(i) in
    children'.(i) <- child
  done;
  (children.(path_idx), !idx_children, new_leaf)

let update k f t =
  let segments = segments (hash k) array_len in
  match find k t with
  | None -> raise Not_found
  | Some v ->
    let new_leaf = leaf k (f v) in
    let children = t.internal in
    let children' = node (Array.make array_len Empty) in
    let _ = List.fold_left (fun (children, children', new_leaf) idx ->
        preserve_pointers children idx children' new_leaf) (children, children', new_leaf) segments in
    {
      length = t.length;
      internal = children'
    }

let dissoc _k _t = ()

let length {length; _} =
  length

(*
open Hamt;;
let m = empty;;
let m1 = assoc "a" 1 m;;
let m2 = assoc "b" 2 m1;;
find "b" m2;;
let m3 = update "b" (fun x -> x + 1) m2;;
find "b" m3;;
*)
