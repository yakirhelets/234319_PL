(* task 1 *)
fun quickSort predicate = fn (xs:'a list) => let
    fun partition (xs:'a list, x:'a) = let
      fun partitionAux (ys, less, more) =
        if null(ys) then (less, more) else
          if predicate(hd(ys),x)
          then partitionAux (tl(ys), less@[hd(ys)], more)
          else partitionAux (tl(ys), less, more@[hd(ys)])
    in
      partitionAux (xs, [], [])
    end
  in
    if List.length(xs) <= 1 then xs else
    let
      val (less, more) = partition (tl xs, hd xs)
    in
      (quickSort predicate less)@[hd xs]@(quickSort predicate more)
    end
  end

(* task 2 *)
fun mergeSort predicate= fn (xs:'a list) => let
    val mid = List.length(xs) div 2
    fun merge (ls, nil) : 'a list = ls
    | merge (nil, rs) = rs
    | merge (ls, rs) =
      if predicate(hd(ls),hd(rs)) then [hd(ls)]@merge(tl(ls), rs)
      else [hd(rs)]@merge(ls, tl(rs))
  in
      merge(mergeSort predicate (List.take(xs, mid)),mergeSort predicate (List.drop(xs, mid)))
  end

(* task 3 *)
fun counter predicate = fn (xs:'a list) => let
    fun loop ([]:'a list) count:int = count
      | loop (xs:'a list) count:int =
        loop (tl xs) (count+1)
  in
      if null(xs) then []
      else let
        val toAdd = [{count=loop(List.filter(fn x => predicate(x,hd(xs))) xs) 0, item=hd(xs)}]
      in
        toAdd@(counter predicate (List.filter(fn x => predicate(x,hd(xs))=false) xs))
      end
  end

(* task 4 *)
(*val  eq_sort  =  fn  :  (string  *  real)  list  ->  (string  *  real)  list*)
fun eq_sort polinom =
  let
    fun smaller ((x1:string,x2:real),(y1:string,y2:real)) = if (x1<y1) then true else false
  in
    quickSort smaller polinom
  end

(* val eqs_sort =
   fn : (string * real) list list -> (string * real) list list *)
fun eqs_sort polinoms = List.map eq_sort polinoms

(* val eq_add =
  fn : (string * real) list -> (string * real) list -> (string * real) list *)
fun eq_add polinom1 polinom2 =
  let
    fun equal ((x1:string,x2:real),(y1:string,y2:real)) = if String.compare(x1,y1)= EQUAL then true else false
    fun greter ((x1:string,x2:real),(y1:string,y2:real)) = if String.compare(x1,y1)= GREATER then true else false
    fun add ((x1:string,x2:real),(y1:string,y2:real)) = (x1,x2+y2)
    fun merge (nil, ys) = ys
    |   merge (xs, nil) = xs
    |   merge (x :: xs, y :: ys) =
          if equal(x,y) then (add(x,y)) :: merge(xs,ys)
          else
          if greter(x,y) then y :: merge(x::xs,ys)
          else x :: merge(xs, y::ys)
  in
    merge (polinom1,polinom2)
  end

(* val eq_multiply =
 fn : (string * real) list -> real -> (string * real) list *)

 fun eq_multiply polinom num =
  let
    fun multi num (x1:string,x2:real) = (x1,x2*num)
  in
    List.map (multi num) polinom
  end


(* val eq_extract =
   fn : (string * real) list -> string -> (string * real) list *)

fun eq_extract polinom str =
  let
    fun equal ((x1:string,x2:real),str) = if String.compare(x1,str)= EQUAL then true else false
    fun notstr str (x1:string,x2:real) = if equal ((x1,x2),str) then false else true
    fun isstr str (x1:string,x2:real) = if (notstr str (x1,x2))=false then true else false
    fun GetStr polinom str = hd(List.filter (isstr str) polinom)
    fun strGetReal (x1:string,x2:real) = x2
    fun polinomWithoutStr str polinom = List.filter (notstr str) polinom
  in
    eq_multiply (polinomWithoutStr str polinom) (~1.0/(strGetReal(GetStr polinom str)))
  end

(* val eq_substitute =
 fn : (string * real) list -> string -> (string * real) list -> (string * real) list *)
 fun eq_substitute polinom str eq =
  let
    fun equal ((x1:string,x2:real),str) = if String.compare(x1,str)= EQUAL then true else false
    fun notstr str (x1:string,x2:real) = if equal ((x1,x2),str) then false else true
    fun isstr str (x1:string,x2:real) = if (notstr str (x1,x2))=false then true else false
    fun IsStrInPol polinom str = if null (List.filter (isstr str) polinom) then false else true
    fun GetStr polinom str = hd(List.filter (isstr str) polinom)
    fun eqGetReal (x1:string,x2:real) = x2
    fun polinomWithoutStr str polinom = List.filter (notstr str) polinom
    fun multi str eq polinom = eq_multiply eq (eqGetReal(GetStr polinom str))
  in
    if ((IsStrInPol polinom str)=false) then polinom
    else eq_add (eq_sort (multi str eq polinom)) (eq_sort (polinomWithoutStr str polinom))
  end


(* val eqs_substitute =
   fn : (string * real) list list -> string -> (string * real) list -> (string * real) list list *)

  fun eqs_substitute polinoms str eq =
    let
      fun polinomsMap (nil,str,eq) = []
        | polinomsMap (x::xs,str,eq) = (eq_substitute x str eq)::(polinomsMap(xs,str,eq))
    in
      polinomsMap (polinoms,str,eq)
    end


(* val eqs_solve_step =
   fn : (string * real) list list -> string -> (string * real) list list *)

fun eqs_solve_step polinoms str =
    let
      fun equal ((x1:string,x2:real),str) = if String.compare(x1,str)= EQUAL then true else false
      fun notstr str (x1:string,x2:real) = if equal ((x1,x2),str) then false else true
      fun isstr str (x1:string,x2:real) = if (notstr str (x1,x2))=false then true else false
      fun IsStrInPol str polinom = if null (List.filter (isstr str) polinom) then false else true
      fun IsStrnotInPol str polinom = if null (List.filter (isstr str) polinom) then true else false

      fun IsStrInPols polinoms str = if null (List.filter (IsStrInPol str) polinoms) then false else true
      fun GetPolinomWithStr polinoms str = hd (List.filter (IsStrInPol str) polinoms)
      fun GetPolinomsWithStr polinoms str = List.filter (IsStrInPol str) polinoms
      fun GetPolinomsWithoutStr polinoms str = (List.filter (IsStrnotInPol str) polinoms)

      fun Newpolinoms polinoms str = (tl(GetPolinomsWithStr polinoms str))@(GetPolinomsWithoutStr polinoms str)
      fun NewpolinomsSort polinoms str = eqs_sort(Newpolinoms polinoms str)

      fun strVal polinoms str = eq_extract (GetPolinomWithStr polinoms str) str

    in
      eqs_substitute (Newpolinoms polinoms str) str (strVal polinoms str)
    end;

  (* val eqs_solve =
       fn : (string * real) list list -> string list -> (string * real) list list *)

fun eqs_solve polinoms [] = polinoms
 | eqs_solve polinoms strs =
   let
         fun equal ((x1:string,x2:real),str) = if String.compare(x1,str)= EQUAL then true else false
         fun strequal (str1,str2) = if String.compare(str1,str2)= EQUAL then true else false
         fun notstr str (x1:string,x2:real) = if equal ((x1,x2),str) then false else true
         fun isstr str (x1:string,x2:real) = if (notstr str (x1,x2))=false then true else false
         fun IsStrnotInPol str polinom = if null (List.filter (isstr str) polinom) then true else false

         fun getElm (x1:string,x2:real) = x1
         fun getVal (x1:string,x2:real) = x2
         fun getEqElm polinom = List.map getElm polinom

         fun getPolinomCounter polinom strs = counter strequal ((getEqElm polinom)@strs)
         fun filterFun {count:int, item:string} = if count=1 andalso (strequal("_",item)=false) then true else false
         fun getNotInStr polinom strs = List.filter filterFun (getPolinomCounter polinom strs)
         fun getElm2 {count:int, item:string} = item
         fun getEqElmfinal strs polinom  = List.map getElm2 (getNotInStr polinom strs)

         fun forAllEq polinoms strs = List.map (getEqElmfinal strs) polinoms
         fun flatten [] = []
           | flatten (x::l) = x @ flatten l
         fun isolate [] = []
           | isolate (x::xs) = x::isolate(List.filter (fn y => y <> x) xs)
         fun getIntresStr polinoms strs = isolate (flatten (forAllEq polinoms strs))


         fun temp polinoms strs = List.map (eqs_solve_step polinoms) (getIntresStr polinoms strs)
         fun DoSteps polinoms strs = if (null (temp polinoms strs)) then polinoms else (hd(temp polinoms strs))
         fun getHof polinoms strs = 1.0/getVal(hd(List.drop(hd(eqs_sort(DoSteps polinoms strs)),1)))
         fun eq_multiply_Aux num polinom  =
          let
            fun multi num (x1:string,x2:real) = (x1,x2*num)
          in
            List.map (multi num) polinom
         end
         fun MakesOneOne polinoms strs = List.map (eq_multiply_Aux (getHof polinoms strs)) (DoSteps polinoms strs)
     in
         MakesOneOne polinoms strs
   end;

   (* val eqs_solutions =
 fn : (string * real) list list -> string list -> (string * real) list *)

 fun eqs_solutions polinoms strs =
   let
     fun TheMainWork polinoms strs = eqs_solve polinoms strs
     fun foo str = [str]
     fun toSrtingList strs = List.map foo strs
     fun getFinal polinoms strs = List.map (eqs_solve(eqs_solve polinoms strs)) (toSrtingList strs)
     fun foo2 nil = []
       | foo2 list = hd(list)
     fun getFinalFinal polinoms strs = List.map foo2 (getFinal polinoms strs)

     fun getStr (x1:string,x2:real) = x1
     fun getStr2 polinoms strs = List.map (fn y=>List.drop(y,1))  (getFinalFinal polinoms strs)
     fun getStr3 polinoms strs = List.map hd (getStr2 polinoms strs)
     fun getStr4 polinoms strs = List.map getStr (getStr3 polinoms strs)

     fun change b (x1:string,x2:real) = (b,x2:real)
     fun func [] [] = []
       | func pol b = change (hd(b)) (hd(eq_extract (hd(pol)) (hd(b)))) :: (func (tl(pol)) (tl(b)))


     fun DONE polinoms strs = func (getFinalFinal polinoms strs) (getStr4 polinoms strs)
     fun equal ((x1:string,x2:real),(x3:string,x4:real)) = String.compare(x1,x3)=EQUAL andalso true
     fun removeDuplicates [] = []
         | removeDuplicates xs = let
            val toAdd = hd (List.filter(fn x => equal(x,hd(xs))) xs)
          in
            [toAdd]@removeDuplicates (List.filter(fn x => (equal(x,hd(xs))=false)) xs)
          end
   in
     removeDuplicates (DONE polinoms strs)
   end;
