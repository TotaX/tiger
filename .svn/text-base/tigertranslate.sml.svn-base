structure tigertranslate :> tigertranslate =
struct
  (*
  *
  *
  * MODULO PARA LA CREACION DE CODIGO INTERMEDIO
  * POR EL MOMENTO USAREMOS ESTE DATATYPE
  *
  * *)
  open tigerstack
  open tigerframe
  open tigertree
  open tigerabs
  structure T = tigertree    (*arbol de codigo intermedio*)
  structure TEMP = tigertemp (*Creaci'on de temporarios y etiquetas*)
  structure F = tigerframe
  
  (*********************************************************)
  (*                     TRANSLATE                         *)
  (*********************************************************)
  datatype exp = SCAF (*luego eliminar est'a para que compile*)
                |Ex of T.exp
                |Nx of T.stm
                |Cx of TEMP.label * TEMP.label -> T.stm
  (*********************************************************)
  (*                    LEVEL                              *)
  (*********************************************************)
  
  type access = tigerframe.access
  type level = {    parent:F.frame option,
                    frame: F.frame,
                    level: int,
                    stack: TEMP.label tigerstack.Stack
               }
 val outermost : level = {    parent = NONE,
                              frame = F.newFrame{name=TEMP.namedlabel "_tigermain"
                                        ,formals=[]},
                              level = 0, (*main tiene level cero como debería ser*)
                              stack = stackNew()
                          }
  fun getLevelParent (l:level) = #parent l
  fun getLevelFrame (l:level)  = #frame l
  fun getLevelLevel (l:level) = #level l
  fun getLevelStack (l:level) = #stack l
  val actualLevel = ref ~1                        (*prev level main*)
  fun getLevel () = !actualLevel                     (*this*)
  fun incrLevel () = actualLevel := !actualLevel + 1 (*in*)
  fun decrLevel () = actualLevel := !actualLevel - 1 (*out*)

  val lstack = stackNew()
  val _ = stackPush outermost lstack 

  fun getTopLevel () =  stackTop lstack
  fun inLevel s = stackPush s lstack 
  fun outLevel () = stackPop lstack
  fun newLevel (name,formals) : level = {
                                  parent = SOME(getLevelFrame(getTopLevel())),
                                  frame = F.newFrame {  name=TEMP.namedlabel name, 
                                                        formals=formals
                                                     },
                                  level = getLevel (),
                                  stack = stackNew () : TEMP.label Stack
                                }


  (*********************************************************)
  (*                    Utilitarios                        *)
  (*********************************************************)
  (*FUNCTION: seq
  * DESCRIPTION: toma una lista de TREE y la convierte
  *             en una secuencia de TREE.SEQ
  * RETURN: Tree.exp                                       *) 
  (*********************************************************)
  fun seq [] = raise Fail "internal error"
    |seq [_] = raise Fail "internal error"
    |seq [a,b] = T.SEQ(a,b)
    |seq (hd::tl) = T.SEQ(hd,seq tl)

  (*********************************************************)


  (*********************************************************)
  (*              unEx : exp -> T.exp                      *)
  (*********************************************************)
  fun unEx (Ex e) = e
   |unEx (Cx genstm) = 
    let
      val r = TEMP.newTemp ()
      val t = TEMP.newLabel () (*true*)
      val f = TEMP.newLabel () (*false*)
    in
      T.ESEQ(seq[T.MOVE(T.TEMP r,T.CONST 1),
                        genstm(t,f),
                        T.LABEL f,
                        T.MOVE (T.TEMP r,T.CONST 0),
                        T.LABEL t],
                 T.TEMP r)
    end
   |unEx (Nx s) = T.ESEQ(s,T.CONST 0)
   |unEx SCAF = raise Fail "unEx SCAF"
  (*********************************************************)
  (*                unNx : exp -> T.stm                    *)
  (*********************************************************)
  fun unNx (Nx s) = s
    |unNx (Ex e) =  T.EXP e
    |unNx (Cx c) =
        let
          val (l1,l2) = (TEMP.newLabel(),TEMP.newLabel())
        in
          seq [c(l1,l2),T.LABEL l1,T.LABEL l2]
        end
   |unNx SCAF = raise Fail "unNx SCAF"
  (*********************************************************)
  (*unCx : exp -> (TEMP.label*TEMP.label->T.stm) *)
  (*********************************************************)
  fun unCx (Cx genstm) = genstm
    |unCx (Ex (T.CONST 0) ) = (fn (_,f) => T.JUMP (T.NAME f,[f]))
    |unCx (Ex (T.CONST _) ) = (fn (v,_) => T.JUMP (T.NAME v,[v]))
    |unCx (Ex e) = (fn (t,f) => T.CJUMP (T.EQ,e,T.CONST 0,f,t))
    |unCx (Nx _ ) = raise Fail "internal error"
    |unCx SCAF = raise Fail "unCx SCAF"

  (*********************************************************)
  (*                  TRANSLATE.UTILS                      *)
  (*********************************************************)
  fun preForWhile() = stackPush (TEMP.newLabel()) (getLevelStack (getTopLevel()))
  fun posForWhile() = stackPop (getLevelStack (getTopLevel()))
  fun allocLocal (f:level) b = 
    let 
          val f' = getLevelFrame(f)
    in 
         F.allocLocal f' b 
    end
  fun pp s = tigertree.tree (unNx s)
  (*********************************************************)
  (*                      TRANSLATE                        *)
  (*********************************************************)
  (* @description: funciones encargardas de realizar       *)
  (* la generacion de codigo intermedio (IR)               *)
  (*********************************************************)
  fun nilExp () = Ex (T.CONST 0)
  fun unitExp () = Ex (T.CONST 0)
  fun intExp (i) = Ex (T.CONST i)
  fun seqExp (s,e) = 
  let
    val s' = List.map unNx s 
    val e' = unEx e
  in
      Ex (T.ESEQ(if ( (List.length s') = 1) then hd s' else seq s',e'))	
  end

  fun breakExp() =
    let
      val l = stackTop (getLevelStack (getTopLevel()))
    in Nx ( T.JUMP (T.NAME l ,[l]) ) end

  fun whileExp (test,body) = 
    let
      val (tr,sigue,fa) =(  TEMP.newLabel(),
                            TEMP.newLabel(),
                            stackTop (getLevelStack (getTopLevel()))

                          )
      val cond = unCx test
    in
      Nx (seq [ T.LABEL sigue,
                cond(tr,fa),
                unNx body,
                T.JUMP (T.NAME sigue,[sigue]),
                T.LABEL fa
              ]
         )
    end

  fun forExp(var,lo,hi,body) =
    let
      val t1 = TEMP.newTemp()
      val v = unEx var
      val (lf,ls,s1) = (   TEMP.newLabel(),
                        stackTop (getLevelStack (getTopLevel())),
                        TEMP.newLabel()
                       )
    in
      Nx ( seq [        T.MOVE (v,unEx lo),
                        T.MOVE (T.TEMP t1, unEx hi),
                        T.CJUMP (T.LE,v,T.TEMP t1,lf,ls),
                        T.LABEL ls,
                        unNx body,
                        T.CJUMP (T.EQ,v,T.TEMP t1,lf,s1),
                        T.LABEL s1,
                        T.MOVE (v,T.BINOP(T.PLUS,v,T.CONST 1)),
                        T.JUMP (T.NAME ls,[ls]),
                        T.LABEL lf
                ]
          )

                
    end

  fun stringExp(s) =
  let
    val l = TEMP.newLabel()
    val _ = F.guarda (F.STRING (l,s))
  in
    Ex (T.NAME l)
  end

  fun simpleVar (acc,level) =
  let
    val acc = case acc of
                 InReg t => Ex(T.TEMP t)
                 |InFrame offset => 
                     Ex (T.MEM (T.BINOP (T.PLUS,T.TEMP fp, T.CONST offset)))
  in
    acc
  end
(* ifExp SIN ELSE es una Nx => SEMANT Else == NONE *)
  fun ifExpNx(test,then') =
    let
      val (t,f,salida) = (TEMP.newLabel(),TEMP.newLabel(),TEMP.newLabel())
      val cond = unCx test
    in
      Nx ( seq  [       cond (t,f),
                        T.LABEL t,
                        unNx then',
                        T.LABEL f
                ]
         )  
    end
(* ifExp con then y else es una Ex *)
  fun ifExpEx (test,then',else') =
  let
    val (t,f,salida) = (TEMP.newLabel(),TEMP.newLabel(),TEMP.newLabel())
    val rt = TEMP.newTemp()
    val cond = unCx test
  in 
    Ex ( T.ESEQ (seq [ cond (t,f),
                      T.LABEL t,(*THEN*)
                      T.MOVE (T.TEMP rt, unEx  then'),
                      T.JUMP (T.NAME salida, [salida]),
                      T.LABEL f,(*ELSE*)
                      T.MOVE (T.TEMP rt, unEx else'),
                      T.LABEL salida
                   ],
               T.TEMP rt
               )
     )
  end

  fun assignExp(var,exp) = Nx (MOVE(unEx var,unEx exp))

  fun buildBinOp (exp1,exp2,oper) = 
           case oper of
                PlusOp => Ex (T.BINOP (T.PLUS ,unEx exp1,unEx exp2))
              |MinusOp => Ex (T.BINOP (T.MINUS ,unEx exp1,unEx exp2))
              |TimesOp => Ex (T.BINOP (T.MUL ,unEx exp1,unEx exp2)) 
              |DivideOp => Ex (T.BINOP (T.DIV, unEx exp1, unEx exp2)) 
              | _ => raise Fail "Internal error buildBinOp"
  
  fun buildCompOp (e1,e2,oper) =
  let
    val (t,f) = (TEMP.newLabel(),TEMP.newLabel())
    val res = TEMP.newTemp()
    fun func op' =
        case op' of
           EqOp => T.EQ
         | NeqOp => T.NE
         | LtOp => T.LT
         | LeOp => T.LE
         | GtOp => T.GT
         | GeOp => T.GE
         | _ => raise Fail "Internal error buildCompOp"
  in
    Ex (T.ESEQ( seq [
                    T.MOVE (T.TEMP res,T.CONST 0),
                    T.CJUMP(func oper, unEx e1,unEx e2,t,f),
                    T.LABEL t,
                    T.MOVE (T.TEMP res,T.CONST 1),
                    T.LABEL f
                    ],
                T.TEMP res
              )
        )
  end 
  fun minusExp(exp) = Ex ( T.BINOP (T.MINUS, T.CONST 0, unEx exp) )
  
  fun varDec (v,i) = T.MOVE (unEx v, unEx i)

  fun letExp (inits,body) = 
    if List.null inits 
        then Ex (unEx body)
        else if ((List.length inits) = 1)
                then Ex (T.ESEQ (List.hd inits,unEx body))
                else Ex (T.ESEQ (seq inits,unEx body)) 

  fun arrayExp(expi,exps) =
  let
    val ret = TEMP.newTemp()
  in
    Ex (
        T.ESEQ ( seq [  
                        T.EXP( 
                                T.CALL (T.NAME (TEMP.namedlabel "_createArray"),
                                                         [unEx expi, unEx exps]
                                       )
                            ),
                        T.MOVE (T.TEMP ret, T.TEMP rv)
                     ],
                 T.TEMP ret
               )
       )
  end

  fun subscriptVar(i,v) =
  let
    val ret = TEMP.newTemp()
    val (t1,t2) = (TEMP.newTemp(),TEMP.newTemp())
  in
   Ex ( T.ESEQ (
                seq [
                        T.MOVE (T.TEMP t1, unEx i),
                        T.MOVE (T.TEMP t2, unEx v),
                        T.EXP ( 
                                T.CALL (
                                        T.NAME (TEMP.namedlabel "_checkIndex"),
                                        [T.TEMP t1, T.TEMP t2]
                                       ) 
                              )
                    ],
                T.MEM (T.BINOP(T.PLUS, T.TEMP t2, 
                                T.BINOP (T.LSHIFT, T.TEMP t1,
                                          T.CONST wordSize))) 
                )
       )
  end

  fun fieldVar(v,n) =
  let
    val tmp = TEMP.newTemp()
  in
    Ex (T.ESEQ(
                seq [
                        T.MOVE (T.TEMP tmp,unEx v),
                        T.EXP(T.CALL (T.NAME (TEMP.namedlabel "_checkNil"),
                                [ T.TEMP tmp ]))
                    ],
                T.MEM(T.BINOP(T.PLUS, T.TEMP tmp,unEx n))
                )
        )
    end

  fun recordExp(lexp) =
  let
    val tmp = TEMP.newTemp()
    val ctos = List.length lexp
  in
    Ex(T.ESEQ(seq [T.EXP(
                        T.CALL(T.NAME (TEMP.namedlabel "_createRecord"), [T.CONST ctos]@
                                (List.map unEx lexp) )),
                       T.MOVE(T.TEMP tmp, T.TEMP rv)],
              T.TEMP tmp
              )
      )         
  end
end
