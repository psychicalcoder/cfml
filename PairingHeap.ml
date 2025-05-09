
type node = {
  mutable value : int;
  mutable children : node HList.hlist;
  mutable siblings : node HList.hlist ref 
}

type contents = Empty | Nonempty of node

type heap = contents ref


let create () =
  ref Empty

let is_empty p =
  !p = Empty


let merge_nodes q1 q2 =
  if q1.value < q2.value
    then (q2.siblings := q1.children ; HList.push q1.children q2 q1 ; q1)
    else (q1.siblings := q2.children ; HList.push q2.children q1 q2 ; q2)

let merge h1 h2 = 
  match !h1, !h2 with
  | Empty, _ -> h2 
  | _, Empty -> h1
  | Nonempty q1, Nonempty q2 -> ref (Nonempty (merge_nodes q1 q2))


let insert p x =
  let rec q2 = ref { value = x; children = HList.create(); siblings = ref (ref HList.Nil) } in
  match !p with
  | Empty -> p := Nonempty !q2
  | Nonempty q1 -> p := Nonempty (merge_nodes q1 !q2)

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
    if HList.is_empty q.children
      then p := Empty
      else p := Nonempty (merge_pairs q.children);
    x

let parent k = 
  let rec itr n = 
    match !(!(n.siblings)) with 
    | HList.Nil -> assert false  
    | HList.Tail(p) -> p
    | HList.Head(x, xs) -> itr x 
  in match !k with 
    | Empty -> assert false 
    | Nonempty q -> itr q

(* let delete_key k = 

let decrease_key k d = 
  let h = ref !k in 
  let p = parent h in 
  let () = h.value = h.value-d in 
  (delete_key k; p.value = p.value-d; merge p) *)

let rec rank_siblings siblings value =
  match !siblings with 
  | HList.Nil -> Printf.sprintf "%d}\n" value
  | HList.Head(x, xs) -> (Printf.sprintf " %d;" value) ^ rank_siblings xs x.value 
  | HList.Tail(_) -> Printf.sprintf "%d}\n" value


let rec ranks n =
  "{rank = same; " ^ (rank_siblings !(n.siblings) n.value) ^ 
  match !(n.children) with 
  | HList.Nil -> "" 
  | HList.Head(x, xs) -> (ranks x)
  | HList.Tail(_) -> ""
    

let dot p =
  let rec indot p = (match !p with 
    | Empty -> ""
    | Nonempty n -> 
        (ranks n) ^ (let rec lin children lastval =
          (match !children with 
          | HList.Nil -> "" 
          | HList.Head(x, xs) -> (Printf.sprintf "%d -> %d [label=\"child\"]\n" lastval x.value) ^ (indot (ref (Nonempty x))) ^ (lin xs x.value)
          | HList.Tail(p) -> Printf.sprintf "%d -> %d[label=\"tail\"]\n \n" lastval p.value) in lin n.children n.value))
    in print_endline (Printf.sprintf "digraph {\n%s}" (indot p))




    
let print_node_value q =
  print_string (Printf.sprintf " %d " q.value)

let rec print_node q = 
  let tail () = print_endline "tail" in 
    (print_node_value q; HList.foreach q.children print_node tail)

let print p =
  match !p with 
  | Empty -> print_endline "Empty"
  | Nonempty q -> print_node q  