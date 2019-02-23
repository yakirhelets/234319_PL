datatype opr = PLUS | MINUS | MULT | DIV
datatype expr = Num of int
              | Var of string
              | BinOp of opr*expr*expr
exception RuntimeError and CompileError

(*count : expr -> int*)
fun count (Num _ | Var _ ) = 1
  | count (BinOp(_,expr1,expr2)) = 1 + count(expr1) + count(expr2)

(*freeVars : expr -> string list*)
fun freeVars (Num _) = []
  | freeVars (Var s) = [s]
  | freeVars (BinOp(_,expr1,expr2)) =
    let
    fun equal (a,b) = if (a=b) then true else false
    fun removeDuplicates [] = []
        | removeDuplicates xs = let
           val toAdd = hd (List.filter(fn x => equal(x,hd(xs))) xs)
         in
           [toAdd]@removeDuplicates (List.filter(fn x => (equal(x,hd(xs))=false)) xs)
         end
    in
    removeDuplicates((freeVars expr1)@(freeVars expr2))
    end

(* val propagate = fn : expr -> expr *)
fun propagate(Num(x))= Num(x)
  |   propagate(Var(y))= Var(y)
  |   propagate(BinOp(opr,exp1,exp2))=
        let
          val exp=BinOp(opr,propagate(exp1),propagate(exp2))
        in
          case exp of
                  BinOp(opr,Num(x),Num(y)) =>
                      Num (case opr of
                          PLUS => x+y
                      |   MINUS => x-y
                      |   MULT => x*y
                      |   DIV => (x div y) handle Div => raise CompileError)
              |  _ => exp
       end


(* call : expr -> int -> int *)
fun call (Var s) x = x
 |  call (Num n) x = raise CompileError
 |  call (BinOp(opr,expr1,expr2)) value =
      let
        fun isOneVar exp = List.length(freeVars exp)=1
        fun newexp exp = propagate exp handle CompileError => raise RuntimeError
        fun teanspose (Var _, x) = Num x
          | teanspose (Num n, _) = Num n
          | teanspose (BinOp(opr,expr1,expr2),x) = BinOp(opr,teanspose(expr1,x),teanspose(expr2,x))
        fun getNum (Num m) = m handle RuntimeError => raise RuntimeError
        val yuval = propagate(teanspose(newexp(BinOp(opr,expr1,expr2)),value)) handle CompileError => raise RuntimeError
      in
        if isOneVar (BinOp(opr,expr1,expr2)) then
          getNum(yuval) handle RuntimeError => raise RuntimeError
        else raise CompileError
      end
