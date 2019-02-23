(* part 1 *)
datatype ('a, 'b) heterolist = A of 'a | List of 'a*('b, 'a) heterolist

(* part 2 *)
fun build4 (x,one,y,two) = List (x, List (one, List (y, A two)));
