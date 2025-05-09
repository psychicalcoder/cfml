
type 'a contents = Nil | Head of int * 'a hlist * 'a hlist | Tail of 'a

and 'a hlist = ('a contents) ref
(*Nil: No parent
  Tail: End of list, there is a parent
  Head: Head of list. Can have child or sibling or neither*)
let is_empty p =
  match !p with
  | Nil -> true
  | Tail(_) -> true 
  | _ -> false

let create () =
    ref Nil

let create_child p =
  ref (Tail p)

let push p x parent = 
  let q = ref !p in
  match !q with 
  | Tail(_) -> p := Head(x, q)
  | Nil -> p := Head(x, ref (Tail(parent)))
  | Head(y, ys) -> p := Head(x, q)

let pop p = 
  let q = ref !p in 
  match !q with 
  | Nil -> assert false 
  | Tail(_) -> assert false 
  | Head(x, r) -> p := !r; x 

let merge p1 p2 = 
  match !p1 with 
  | Nil -> p2 
  | Tail(p) -> assert false 
  | Head(v1, _, _) -> 
      match !p2 with
      | Nil -> p1 
      | Tail(p0) -> assert false 
      | Head(v2, _, _) -> if v1 > v2 then push p1  

let rec foreach p f t = 
  match !p with 
  | Nil -> ()
  | Tail(_) -> t ()
  | Head (x,q) -> f x; foreach q f t
  
      
  