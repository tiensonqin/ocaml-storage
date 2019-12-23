(* TODO: add `length` field *)
(* TODO: draw *)
(* TODO: next array should be fixed to utilize cache, is that right? But that will have impose
   a hard limit on the height of the list (which impacts the keys).
*)
(* TODO: lockfree version branch (should be based on ocaml multicore ) *)

type 'a t = {
  mutable height: int;
  head: 'a node
}
and 'a node = {
  key: 'a option;
  next: 'a node array
}

let create height =
  let head = {key = None; next = [||]} in
  {height; head}

let height t =
  t.height

(* Find the first key that's greater than or equal to x. *)
let find x {head; height} =
  let search = Some x in
  let level = height - 1 in
  let rec loop ({next; _} as node) level =
    let node' = next.(level) in
    let key' = node'.key in
    if level == 0 then
      key'
    else if key' >= search then
      loop node (level - 1)
    else
      loop node' level
  in loop head level

(* Assign a random height for the new insert key. *)
let random_height max_height =
  let rec loop height =
    if height < max_height && Random.int(100) mod 4 == 0 then
      loop (height + 1)
    else
      height
  in
  loop 1

let insert x {head; height} =
  "TODO"
