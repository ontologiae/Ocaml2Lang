module L = BatList;;

let f1 ll = L.unique (L.flatten ll);;

let f2 l = (f1 l)@["ioio"]
