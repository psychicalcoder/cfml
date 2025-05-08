
type node = {
  mutable value : int;
  mutable sub : node HList.hlist }

type contents = Empty | Nonempty of node

type heap = contents ref


let create () =
  ref Empty

let is_empty p =
  !p = Empty


let merge_nodes q1 q2 =
  if q1.value < q2.value
    then (HList.push q1.sub q2 q2.sub; q1)
    else (HList.push q2.sub q1 q1.sub; q2)

let merge h1 h2 = 
  match !h1, !h2 with
  | Empty, _ -> h2 
  | _, Empty -> h1
  | Nonempty q1, Nonempty q2 -> ref (Nonempty (merge_nodes q1 q2))



let insert p x =
  let q2 = { value = x; sub = HList.create() } in
  match !p with
  | Empty -> p := Nonempty q2
  | Nonempty q1 -> p := Nonempty (merge_nodes q1 q2)

let rec merge_pairs l =
  let q1 = HList.pop l in
  if HList.is_empty l then q1 else
  let q2 = HList.pop l in
  let q = merge_nodes q1 q2 in
  if HList.is_empty l
     then q
     else merge_nodes q (merge_pairs l)

let pop_min p =
  match !p with
  | Empty -> assert false
  | Nonempty q ->
    let x = q.value in
    if HList.is_empty q.sub
      then p := Empty
      else p := Nonempty (merge_pairs q.sub);
    x


let print_node_value q =
  print_string (Printf.sprintf " %d " q.value)

let rec print_node q = 
  let tail () = print_endline "tail" in 
    (HList.foreach q.sub print_node_value tail; HList.foreach q.sub print_node tail)

let print p =
  match !p with 
  | Empty -> print_endline "Empty"
  | Nonempty q -> print_node q  