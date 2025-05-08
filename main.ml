let x  = PairingHeap.create () in 
let () = PairingHeap.insert x 5 in 
let () = PairingHeap.insert x 6 in 
let () = PairingHeap.insert x 7 in 
let y = PairingHeap.create () in 
let () = PairingHeap.insert y 1 in 
let () = PairingHeap.insert y 2 in 
let () = PairingHeap.insert x 3 in 
PairingHeap.print (PairingHeap.merge x y)