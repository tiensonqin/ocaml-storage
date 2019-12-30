open Skiplist

(* TODO: verify performance and space *)

let sorted =
  QCheck.Test.make ~count:1000
    ~name:"sorted"
    QCheck.(list int)
    (fun l ->
       let t = Int_skiplist.from_list l in
       let l' = Int_skiplist.to_list t in
       (* ensures that no duplication *)
       Base.List.is_sorted_strictly l' ~compare:Base.Int.compare)

let max_height_limit =
  QCheck.Test.make ~count:10
    ~name:"max_height_limit"
    QCheck.(list_of_size (Gen.int_range 10000 20000) int)
    (fun l ->
       let t = Int_skiplist.from_list ~max_height:3 l in
       Int_skiplist.height t <= 3)

let no_duplication =
  QCheck.Test.make ~count:10
    ~name:"no_duplication"
    QCheck.(list (int_range 10 10))
    (fun l ->
       let t = Int_skiplist.from_list l in
       Int_skiplist.length t <= 1)

let print_list l = List.iter (Printf.printf "%d; ") l

let delete =
  QCheck.Test.make ~count:1000
    ~name:"delete"
    QCheck.(list small_int)
    (fun l ->
       let dl = Base.List.dedup_and_sort l ~compare:Base.Int.compare in
       let _ = print_list l in
       let t = Int_skiplist.from_list l in
       let _ = List.iter (fun x -> Int_skiplist.delete t x) dl in
       Int_skiplist.length t == 0)

let () =
  let open Alcotest in
  let suite =
    List.map QCheck_alcotest.to_alcotest
      [
        sorted;
        max_height_limit;
        no_duplication;
        delete;
      ]
  in
  run "Skiplist" [
    "suite", suite
  ]
