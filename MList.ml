
type 'a contents = Nil | Cons of 'a * 'a mlist
and 'a mlist = ('a contents) ref

let is_empty p =
  match !p with
  | Nil -> true
  | Cons (x,q) -> false

let create () =
  ref Nil

let push p x =
  let q = ref !p in
  p := Cons (x, q)

let pop p =
  match !p with
  | Nil -> assert false
  | Cons (x,q) -> p := !q; x

let head p = 
  match !p with 
  | Nil -> assert false 
  | Cons (x,q) -> x 

let tail p = 
  match !p with 
  | Nil -> assert false 
  | Cons (x,q) -> q

let rec foreach p f = 
  match !p with 
  | Nil -> ()
  | Cons (x,q) -> f x; foreach q f 

let print p = 
  print_string "MList ("; 
  let f x = print_string (Printf.sprintf " %d; " x) in foreach p f; 
  print_string ")\n"

let rec from_list p = 
  match p with 
  | [] -> ref Nil 
  | x :: xs -> ref (Cons(x, from_list xs));