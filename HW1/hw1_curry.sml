(*hw1_curry.sml*)

val curry = fn f => fn x => fn y => f(x, y);
val uncurry = fn f => fn (x, y) => f x y;
