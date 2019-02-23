(*yakir helets 305028441 yakirh@campus.technion.ac.il*)
(*yuval isaschar 313362097 isaschar@campus.technion.ac.il*)

(* Task 3 *)
datatype 'a seq = nil
| Cons of ('a) * (unit-> 'a seq);

fun head(Cons(x,_)) = x;

fun tail(Cons(_,xf)) = xf();

(* subtask 1 *)
fun from_fun func = let
    fun from_fun_aux func x = Cons(func(x), fn() => from_fun_aux func (x+1))
  in
    from_fun_aux func 0
  end;
(*
(* examples - subtask 1 *)
from_fun (fn x => 2 * x);
(*val it = Cons (0,fn) : int seq*)
tail it;
(*val it = Cons (2,fn) : int seq*)
tail it;
(*val it = Cons (4,fn) : int seq*)
tail it;
(*val it = Cons (8,fn) : int seq*)

from_fun (fn x => "just a string");
(*val it = Cons ("just a string",fn) : string seq*)
tail it;
(*val it = Cons ("just a string",fn) : string seq*)
tail it;
(*val it = Cons ("just a string",fn) : string seq*)
tail it;
(*val it = Cons ("just a string",fn) : string seq*) *)

(* Part 2 *)
(* note: everey valid solution is םכ the form 27n-2, n={1,2,...}
  =27n+25, n={0,1,2,...} (proof not included ;-) ) *)
fun three_fishermen() = from_fun (fn x => 27*x+25);
(*
(* examples - part 2 *)
three_fishermen();
(*val it = Cons (25,fn) : int seq*)
tail it;
(*val it = Cons (52,fn) : int seq*)
tail it;
(*val it = Cons (79,fn) : int seq*)
tail it;
(*val it = Cons (106,fn) : int seq*) *)


(* Task 4 *)

(* subtask 1 *)
datatype 'a infTable = TNil
| Table of ('a) * (unit-> 'a infTable) * (unit-> 'a infTable);

(* subtask 2 *)
fun value TNil = raise Empty
  | value (Table(x,_,_)) = x;
fun right TNil = raise Empty
  | right (Table(_,_,xf)) = xf();
fun down TNil = raise Empty
  | down (Table(_,yf,_)) = yf();

(* subtask 3 *)
fun gen_table (func:(int*int->'a)) = let
    fun gen_table_aux func (x,y) = Table(func((x,y)), fn() => gen_table_aux func (x,y+1)
    ,fn() => gen_table_aux func (x+1,y))
  in
    gen_table_aux func (0,0)
  end;
(*
(* Examples *)
val mult_table = gen_table (fn (x,y) => (x + 1) * (y + 1));
(* val mult_table = Table (1,fn,fn) : int infTable *)
right (down mult_table);
(* val it = Table (4,fn,fn) : int infTable *)
right (right (down it));
(* val it = Table (12,fn,fn) : int infTable *) *)

(* subtask 4 *)
fun row table = fn num => let
    fun get_row table = fn num => if (num>0)
      then get_row (down(table)) (num-1) else table;
    fun to_sequence table = Cons(value(table), fn x => to_sequence (right table));
  in
    if (num<0) then (raise Empty) else to_sequence (get_row table num)
  end;
(*
(* Examples *)
(* val row = fn : 'a infTable -> int -> 'a seq *)
row mult_table 5;
(* val it = Cons (6,fn) : int seq *)
tail it;
(* val it = Cons (12,fn) : int seq *)
tail it;
(* val it = Cons (18,fn) : int seq *)
tail it;
(* val it = Cons (24,fn) : int seq *) *)

(* subtask 5 *)
fun col table = fn num => let
    fun get_col table = fn num => if (num>0)
      then get_col (right(table)) (num-1) else table;
    fun to_sequence table = Cons(value(table), fn x => to_sequence (down table));
  in
    if (num<0) then (raise Empty) else to_sequence (get_col table num)
  end;
(*
(* Examples *)
(* val col = fn : 'a infTable -> int -> 'a seq *)
col mult_table 7;
(* val it = Cons (8,fn) : int seq *)
tail it;
(* val it = Cons (16,fn) : int seq *)
tail it;
(* val it = Cons (24,fn) : int seq *)
tail it;
(* val it = Cons (32,fn) : int seq *) *)

(* subtask 6 *)
fun diagonal table = let
    fun to_sequence table =
      Cons(value(table), fn x => to_sequence (down (right table)));
  in
    to_sequence (table)
  end;
(*
(* Examples *)
(* val diagonal = fn : 'a infTable -> 'a seq *)
diagonal mult_table;
(* val it = Cons (1,fn) : int seq *)
tail it;
(* val it = Cons (4,fn) : int seq *)
tail it;
(* val it = Cons (9,fn) : int seq *)
tail it;
(* val it = Cons (16,fn) : int seq *)
tail it;
(* val it = Cons (25,fn) : int seq *) *)

(* subtask 7 *)
fun mapt func = fn table => let
    fun new_val func = fn table =>
      Table (func(value(table)), fn() => new_val func (down(table))
      ,fn() => new_val func (right(table)))
  in
    new_val func table
  end;
(*
(* Examples *)
(* val mapt = fn : ('a -> 'b) -> 'a infTable -> 'b infTable *)
mapt (fn x => 25 + x * 27) mult_table;
(* val it = Table (52,fn,fn) : int infTable *)
right it;
(* val it = Table (79,fn,fn) : int infTable *)
right it;
(* val it = Table (106,fn,fn) : int infTable *)
right it;
(* val it = Table (133,fn,fn) : int infTable *)
down it;
(* val it = Table (241,fn,fn) : int infTable *) *)

(* subtask 8 *)
fun transp table = Table (value(table), fn() => (transp (right(table)))
      ,fn() => (transp (down(table))));
(*
(* Examples *)
val pow_table = gen_table (fn (x, y) => Math.pow(real (x + 1), real (y + 1)));
right pow_table;
(* val it = Table (2.0,fn,fn) : real infTable *)
down it;
(* val it = Table (4.0,fn,fn) : real infTable *)
down it;
(* val it = Table (8.0,fn,fn) : real infTable *)
transp pow_table;
(* val it = Table (1.0,fn,fn) : real infTable *)
right it;
(* val it = Table (1.0,fn,fn) : real infTable *)
right it;
(* val it = Table (1.0,fn,fn) : real infTable *)
down it;
(* val it = Table (8.0,fn,fn) : real infTable *)
transp pow_table;
(* val it = Table (1.0,fn,fn) : real infTable *)
right it;
(* val it = Table (1.0,fn,fn) : real infTable *)
down it;
(* val it = Table (4.0,fn,fn) : real infTable *)
down it;
(* val it = Table (9.0,fn,fn) : real infTable *) *)
