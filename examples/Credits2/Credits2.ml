
let incr r =
  r := !r + 1

let rec sum p m =
  if m > 0 then (
    incr p;
    sum p (m-1)
  )
