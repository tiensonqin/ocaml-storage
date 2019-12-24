(* TODO: is it space wasting? how to verify it? *)
(* TODO: add `length` field *)
(* TODO: draw *)
(* TODO: the next vector could be limited to a fixed length to utilize cache, is that right? But it imposes
   a hard limit on the height of the list (which impacts the keys).
*)
(* TODO: lockfree version branch (should be based on ocaml multicore ) *)

module Vector = struct
  include Containers.Vector

  let safe_get v i default =
    if length v > i then
      get v i
    else
      default

  let safe_set v i value =
    if i < length v then
      set v i value
    else
      push v value

  let empty v =
    size v == 0

  (* TODO: customize *)
  (* let vector_to_yojson t =  *)
end

type 'a t = {
  mutable height: int;
  max_height: int;
  head: 'a node
}
and 'a node = {
  key: 'a option;
  next: 'a node ref Vector.vector
}

let empty_node () : int node =
  {key = None; next = Vector.create()}

let create max_height =
  let end_node_ref = ref (empty_node ()) in
  let next = Vector.create () in
  let _ = Vector.push next end_node_ref in
  {height = 1; max_height; head = {key = None; next}}

let create_node x _height =
  {
    key = Some x;
    next = Vector.create()
  }

let height t =
  t.height

let is_end {key; next} =
  Option.is_none key && Vector.empty next

let print_prev prev =
  let _ = Printf.printf "prev: " in
  let _ = Array.iteri (fun i node_ref ->
      match !node_ref.key with
      | None -> ()
      | Some x ->
        Printf.printf "L%d: %d; " i x)
      prev in
  print_newline ()

let print_search_path search_path =
  let _ = Printf.printf "Search path: head" in
  let _ = Vector.iter (fun node ->
      match node.key with
      | None -> ()
      | Some x ->
        Printf.printf " -> %d" x)
      search_path in
  print_newline ()

(* Find the first key that's greater than or equal to x. *)
let find {head; height; _} ?(prev = None) ?(search_path = None) x =
  let search = Some x in
  let level = height - 1 in
  let rec loop ({next; _} as node) level =
    let _ = match search_path with
      | None -> ()
      | Some search_path ->
        Vector.push search_path node in
    let _ = match prev with
      | None -> ()
      | Some prev ->
        prev.(level) := node in
    (* Printf.printf "level: %d\n" level; *)
    let { contents = node' } = Vector.get next level in
    let key' = node'.key in
    (* Option.iter (fun key -> Printf.printf "next key: %d\n" key) key'; *)
    if Option.equal (==) search key' then (
      if level == 0 then node' else loop node (level - 1)
    ) else (
      if (search > key') && (not (is_end node')) then (
        (* Option.iter (fun key -> Printf.printf "horizontal move key: %d\n" key) key'; *)
        loop node' level
      ) else (
        if level == 0 then
          node'
        else
          loop node (level - 1)
      )
    )
  in
  let result = loop head level in
  let _ = match search_path with
    | None -> ()
    | Some search_path ->
      print_search_path search_path in
  let _ = match prev with
    | None -> ()
    | Some prev ->
      print_prev prev in
  result

(* Assign a random height for the new insert key. *)
let random_height max_height =
  let rec loop height =
    let my_max_int = (1 lsl 30) - 1 in
    if height < max_height && Random.int(my_max_int) mod 4 == 0 then
      loop (height + 1)
    else
      height
  in
  loop 1

let insert t x =
  let x_height = random_height t.max_height in
  (* How to avoid this allocation? *)
  let prev = Array.make t.max_height (ref (empty_node ())) in
  let _ = for i = 0 to t.max_height - 1 do
      prev.(i) <- (ref (empty_node ()))
    done in
  let search_path = Vector.create() in
  let _ = find t ~prev:(Some prev) ~search_path:(Some search_path) x in
  (* let _ = print_prev prev in *)
  if x_height > t.height then (
    (* Printf.printf "New height: %d, old height: %d\n" x_height t.height; *)
    let head_ref = (ref t.head) in
    for i = t.height to x_height - 1 do
      Array.set prev i head_ref
    done;
    t.height <- x_height
  );
  let x_node = create_node x x_height in
  let x_node_ref = ref x_node in
  for i = 0 to x_height - 1 do
    let node_ref = prev.(i) in
    Vector.safe_set x_node.next i (Vector.safe_get !node_ref.next i (ref (empty_node ())));
    Vector.safe_set !node_ref.next i x_node_ref
  done

let delete t x =
  let prev = Array.make t.max_height (ref (empty_node ())) in
  let _ = for i = 0 to t.max_height - 1 do
      prev.(i) <- (ref (empty_node ()))
    done in
  let search_path = Vector.create() in
  let node = find t ~prev:(Some prev) ~search_path:(Some search_path) x in
  let _ = print_prev prev in
  for i = Vector.size node.next - 1 downto 0 do
    let prev_ref = prev.(i) in
    let next_ref = Vector.get node.next i in
    Vector.safe_set !prev_ref.next i next_ref;
    if Option.is_none !prev_ref.key && Option.is_none !next_ref.key then
      t.height <- t.height - 1
  done

let fold_left t f acc =
  let rec loop {next; _} acc =
    let node_ref = Vector.get next 0 in
    match !node_ref.key with
    | None -> acc
    | Some x ->
      loop !node_ref (f acc x)
  in
  loop t.head acc

let to_list t =
  fold_left t (fun acc x -> x :: acc) []
  |> List.rev

let length t =
  fold_left t (fun acc _x -> acc + 1 )  0

(*
print t ->
height: 3, max_height: 12, length: 10
L2 head -> 4 -> 5 -> tail
L1 head -> 4 -> 5 -> tail
L0 head -> 0 -> 1 -> 2 -> 3 -> 4 -> 5 -> 6 -> 7 -> 8 -> 9 -> tail
*)
let print ({height; head; max_height} as t) =
  let _ = Printf.printf "height: %d, max_height: %d, length: %d\n" height max_height (length t) in
  for i = height - 1 downto 0 do
    let _ = Printf.printf "L%d head -> " i in
    let rec loop {key; next} =
      let _ = match key with
        | None -> ()
        | Some key -> Printf.printf "%d -> " key in
      if Vector.empty next then
        print_endline "tail"
      else
        let next_node = !(Vector.get next i) in
        loop next_node
    in
    loop head
  done

(*
Printexc.record_backtrace true;;
open Skiplist;;
let t = create 12;;

let l = [1;0;9;7;8;6;4;2;3;5];;
List.iter (fun x -> insert t x) l;;
length t;;
print t;;

let search_path = Vector.create();;
let prev = Array.make t.max_height (ref (empty_node ()));;
  for i = 0 to t.max_height - 1 do
      prev.(i) <- (ref (empty_node ()))
    done;;
   find t ~search_path:(Some search_path) ~prev:(Some prev) 3;;
*)
