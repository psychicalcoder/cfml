
type 'a contents = Nil  | Head of 'a * 'a hlist * 'a hlist | Tail of 'a hlist
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

let create_child parent x =
  ref (Tail parent)

let push p x subtree = 
  let q = ref !p in
  match !q with 
  | Tail(parent) -> p := Head(x, subtree, q)
  | Nil -> p := Head(x, subtree, ref (Tail(p)))
  | Head(y, m, ys) -> p := Head(x, subtree, q)

let pop p = 
  let q = ref !p in 
  match !q with 
  | Nil -> assert false 
  | Tail(_) -> assert false 
  | Head(x, m, r) -> p := !r; x 

let rec foreach p f t = 
  match !p with 
  | Nil -> ()
  | Tail(_) -> t ()
  | Head (x,m,q) -> f x; foreach q f t
  
      
  