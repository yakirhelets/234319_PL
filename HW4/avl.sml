(*yakir helets 305028441 yakirh@campus.technion.ac.il*)
(*yuval isaschar 313362097 isaschar@campus.technion.ac.il*)

datatype 'a AVLTree = Nil | Br of ((int*('a))*('a AVLTree)*('a AVLTree));
datatype Balance = RR | LR | LL | RL;
exception NotFound;

(*val size = fn: 'a AVLTree->int*)
fun size Nil= 0
  | size (Br(v,tl,tr)) = 1 + size tl + size tr;

(* val insert = fn: ('a AVLTree*(int*'a))->'a AVLTree *)
fun insert(Nil,(iind,ival)) = Br((iind,ival),Nil,Nil)
  | insert(t as Br((vind,vval),lt,rt),(iind,ival)) =
    let
      fun height Nil = 0
        | height(Br(_,lt,rt)) = 1 + Int.max(height lt, height rt);
      fun rotLeft(Br(v,Br(vl,llt,lrt),rt)) = Br(vl,llt, Br(v,lrt,rt))
      fun rotRight(Br(v,lt,Br(vr,rlt,rrt))) = Br(vr,Br(v,lt,rlt), rrt)
      fun dRotLeftRight(Br(v,Br(vl,llt,Br(vlr,lrlt,lrrt)),rt)) =
                                     Br(vlr, Br(vl,llt,lrlt),Br(v,lrrt,rt))
      fun dRotRightLeft(Br(v,lt,Br(vr,Br(vrl,rllt,rlrt),rrt))) =
                                     Br(vrl,Br(v,lt,rllt),Br(vr,rlrt,rrt))
    in
       case Int.compare(iind, vind) of
           LESS    => let val nlt as Br((nvlin,nvl),_, _) = insert(lt,(iind,ival))
                          val nt = Br((vind,vval),nlt,rt)
                      in if height nlt - height rt = 2
                         then if iind < nvlin then rotLeft nt
                              else dRotLeftRight nt
                         else nt
                      end
         | EQUAL   => t
         | GREATER => let val nrt as Br((nvrin,nvr),_, _) = insert(rt,(iind,ival))
                          val nt = Br((vind,vval),lt,nrt)
                      in if height nrt - height lt = 2
                         then if iind > nvrin then rotRight nt
                              else dRotRightLeft nt
                         else nt
                      end
    end

(* val remove = fn: ('a AVLTree*int)->'a AVLTree *)
fun remove(Nil, _) = Nil
  | remove(Br((iind,ival),t1,t2), j) =
  let
    fun height Nil = 0
      | height(Br(_,lt,rt)) = 1 + Int.max(height lt, height rt);
    fun rotLeft(Br(v,Br(vl,llt,lrt),rt)) = Br(vl,llt, Br(v,lrt,rt))
    fun rotRight(Br(v,lt,Br(vr,rlt,rrt))) = Br(vr,Br(v,lt,rlt), rrt)
    fun dRotLeftRight(Br(v,Br(vl,llt,Br(vlr,lrlt,lrrt)),rt)) =
                                 Br(vlr, Br(vl,llt,lrlt),Br(v,lrrt,rt))
    fun dRotRightLeft(Br(v,lt,Br(vr,Br(vrl,rllt,rlrt),rrt))) =
                                 Br(vrl,Br(v,lt,rllt),Br(vr,rlrt,rrt))
    fun get (Nil,i) = raise NotFound
      | get ((Br((j,jval),t1,t2)),i) = case Int.compare(i,j) of
          EQUAL   => jval
        | LESS    => get(t1,i)
        | GREATER => get(t2,i)
    fun balLeftRight(t as Br(_,Br(_,llt,lrt),rt)) =
                 if height llt >= height lrt
                 then rotLeft t
                 else dRotLeftRight t
    fun balRightLeft(t as Br(v,lt,Br(vr,rlt,rrt))) =
                 if height rrt >= height rlt
                 then rotRight t
                 else dRotRightLeft t
    fun min Nil = raise NotFound
      | min(Br((iind, vind),Nil,_)) = iind
      | min(Br(_,t,_))  = min t
    fun max Nil = raise  NotFound
               | max(Br((iind, vind),_,Nil)) = iind
               | max(Br(_, _, t))  = max t
  in
    case Int.compare(iind,j) of
         LESS    => let val nt2 = remove(t2,j)
                        val nt = Br((iind,ival),t1,nt2)
                    in if height t1 = height nt2 + 2
                       then balLeftRight nt
                       else nt
                    end
       | GREATER => let val nt1 = remove(t1, j)
                        val nt = Br((iind,ival),nt1,t2)
                    in if height nt1 + 2 = height t2
                       then balRightLeft nt
                       else nt
                    end
       | EQUAL   => (case (t1, t2) of
                          (Nil , _) => t2
                        | (_,  Nil) => t1
                        |  _  => if height t1 <= height t2
                                      then let val m = min t2
                                           in Br((m,get(t2,m)),t1,remove(t2, m)) end
                                      else let val m = max t1
                                           in Br((m,get(t1,m)),remove(t1, m),t2) end)
    end


(* val get = fn: ('a AVLTree*int)->`a *)
fun get (Nil,i) = raise NotFound
  | get ((Br((j,jval),t1,t2)),i) = case Int.compare(i,j) of
      EQUAL   => jval
    | LESS    => get(t1,i)
    | GREATER => get(t2,i)

(* val inorder = fn: ('a AVLTree)->`a list *)
fun inorder Nil = []
  | inorder (Br(item, left, right)) =
      let
        fun getvalue (key,value) = value
        val v = getvalue item
      in
        inorder left @ [v] @ inorder right
      end
