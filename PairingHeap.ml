
type node = {
  mutable value : int;
  child : contents ref;
  sibling : contents ref;
  parent : contents ref 
} and contents = Empty | Nonempty of node

type heap = contents ref

let create () =
  ref Empty

let is_empty p =
  !p = Empty

let merge_nodes q1 q2 =
  if q1.value < q2.value
    then (q2.parent := (match !(q1.child) with | Empty -> Nonempty q1 | Nonempty _ -> Empty); q2.sibling := !(q1.child) ; q1.child := Nonempty q2 ; q1)
    else (q1.parent := (match !(q2.child) with | Empty -> Nonempty q2 | Nonempty _ -> Empty); q1.sibling := !(q2.child) ; q2.child := Nonempty q1 ; q2)

let merge h1 h2 = 
  match !h1, !h2 with
  | Empty, _ -> h2 
  | _, Empty -> h1
  | Nonempty q1, Nonempty q2 -> ref (Nonempty (merge_nodes q1 q2))


let insert p x =
  let rec q2 = { value = x; child = ref Empty; sibling = ref Empty; parent = ref Empty } in
  match !p with
  | Empty -> p := Nonempty q2
  | Nonempty q1 -> if is_empty q1.child then q2.parent := !p; p := Nonempty (merge_nodes q1 q2)


let rec merge_siblings q =
  match !(q.sibling) with 
  | Empty -> q 
  | Nonempty q1 -> let q2 = merge_nodes q q1 in
    match !(q1.sibling) with 
    | Empty -> q2
    | Nonempty q3 -> merge_nodes q2 (merge_siblings q3)

let pop_min p =
  match !p with
  | Empty -> assert false
  | Nonempty q ->
    let x = q.value in
    (match !(q.child) with
    | Empty -> p := Empty
    | Nonempty child -> p := Nonempty (merge_siblings child));
    x

let rec rank_siblings q  =
  match !(q.sibling) with 
  | Empty -> Printf.sprintf "%d}\n" q.value
  | Nonempty sib -> (Printf.sprintf " %d; " q.value) ^ rank_siblings sib 

let rec ranks n =
  "   {rank = same; " ^ (rank_siblings n) ^ 
  (match !(n.child) with 
  | Empty -> ""
  | Nonempty child -> (ranks child)) ^ 
  (match !(n.sibling) with 
  | Empty -> ""
  | Nonempty sib -> (ranks sib))

let dot h =
  match !h with 
  | Empty -> print_endline ""
  | Nonempty q -> 
    let rec recurser p lastval is_sib = 
    (match !p with
      | Empty -> ""
      | Nonempty {value = v; child = c; sibling = s; parent = p} -> 
        ((Printf.sprintf "%d -> %d [label=\"%s\"]\n" lastval v (if is_sib then "sibling" else "child")) ^ 
        (recurser c v false) ^
        (recurser s v true)) ^
        (match !p with 
        | Empty -> ""
        | Nonempty daddy -> Printf.sprintf "%d -> %d [label=\"parent\"]\n" v daddy.value)
    ) in print_endline (Printf.sprintf "digraph {\n%s}" (ranks q ^ (recurser q.child q.value false)))




