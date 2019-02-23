(*task 1 - should be working*)
fun balance (x) =
  let
    fun balanceCheck ([],bal) = if bal <> 0 then false else true
        | balanceCheck (x::xs,bal) = if (bal < 0) then false
                                else (case x of
                                      #"(" => balanceCheck(xs,bal + 1)
                                    | #")" => balanceCheck(xs,bal -1)
                                    | _ => balanceCheck(xs,bal))
    in
      balanceCheck(explode x ,0)
    end;

(*task 2*)
fun perfect number =
  let
    fun isDivisor (x,number) = if (number mod x = 0) then true else false
    fun divisorsSum (1,i,result) = 0
      | divisorsSum (number,i,result) =
      if (i>floor(Math.sqrt(real(number)))) then result else
      if (isDivisor(i,number)) then divisorsSum(number,i+1,result+i+(number div i)) else
      divisorsSum(number,i+1,result)
  in
      (divisorsSum(number,2,1)=number)
  end;


(*task 3*)
(*'a->'b->('a * 'b ->'b)->'b *)
fun sig1 a = fn b => fn foo => foo(a,foo(a,b));
(*int * real -> (real -> string) -> bool *)
fun sig2 (num,rnum) = fn toString => toString(real(num)*Math.sqrt(rnum)) = "some string";
(*('a -> 'b -> 'c) -> 'a -> 'b -> 'd -> 'c *)
fun sig3 a = fn b => fn c => fn d => a b c;
(*'a -> 'b -> int -> int -> int *)
fun sig4 a = let
    fun foo c = fn b => if c+2>b+3 then 3 else 4
  in
    fn d => foo
  end;
(* ('a -> 'b) -> 'a -> ('b * 'b -> 'c) -> 'c *)
fun sig5 a b foo = foo ((a b),(a b));
(*unit -> unit -> int *)
fun sig6 () = fn () => 1;
(* ('a list -> 'b) -> 'a list -> 'a list -> 'b *)
fun sig7 f list1 [] = f(list1)
  | sig7 f [] list2 = f(list2)
  | sig7 f list1 list2 = f(list1);

(*task 4*)
  fun subSums xs =
    let
      fun take (0,_)=[]
        | take (i,x::xs) = x::take(i-1,xs)
      fun sumList (nil) = 0
        | sumList(xs) = hd (xs) + sumList (tl xs)
      fun subSumsAux (xs,i) =
        if (i > List.length(xs)) then [] else
        sumList(take(i,xs)) :: subSumsAux(xs,i+1)
    in
      subSumsAux(xs,1)
    end;

(*task 5*)
fun isPalindrom(s:string) : bool = let
    fun reverseAux(s:string, i:int) : string =
      if i = 1 then str(String.sub(s, 0))
      else str(String.sub(s, i-1)) ^ reverseAux(s, i-1)
    fun reverseString(s:string) : string =
      if size(s)=0 then "" else reverseAux(s, size(s))
  in
    reverseString(s)=s
  end;
