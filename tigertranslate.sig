signature tigertranslate =
sig
	type level = 	{    	parent:tigerframe.frame option,
				frame: tigerframe.frame,
				level: int,
			        stack: tigertemp.label tigerstack.Stack
        	       	}
	type access
	val outermost : level
  (*********************************************************)
  (*                     TRANSLATE                         *)
  (*********************************************************)
  datatype exp = SCAF (*luego eliminar est'a para que compile*)
                |Ex of tigertree.exp
                |Nx of tigertree.stm
                |Cx of tigertemp.label * tigertemp.label -> tigertree.stm

(************************************)
(*            SEMANT                *)
(************************************)
(*           TRANSLATE              *)
(************************************)
(***************) (*****************)
(*   FRAME     *) (*      TEMP     *)
(***************) (*****************)

val allocLocal : level -> bool -> access
val getLevelParent : level -> tigerframe.frame option
val incrLevel : unit -> unit
val decrLevel : unit -> unit
val getLevel : unit -> int
val getTopLevel : unit -> level
val newLevel : string*bool list -> level
val preForWhile : unit -> unit
val posForWhile : unit -> tigertemp.label

val unEx : exp -> tigertree.exp
val unNx : exp -> tigertree.stm
val unCx : exp -> (tigertemp.label*tigertemp.label -> tigertree.stm)
val nilExp : unit -> exp
val unitExp : unit -> exp
val intExp : int -> exp
val breakExp : unit -> exp
val stringExp : string -> exp
val seqExp : exp list*exp -> exp
val whileExp : exp*exp-> exp
val simpleVar : access*int -> exp
val forExp : exp*exp*exp*exp -> exp
val ifExpNx : exp*exp->exp
val ifExpEx : exp*exp*exp -> exp
val assignExp : exp*exp -> exp
val buildBinOp: exp*exp*tigerabs.oper-> exp
val minusExp : exp -> exp
val varDec : exp*exp -> tigertree.stm
val letExp : tigertree.stm list*exp -> exp
val buildCompOp : exp*exp*tigerabs.oper -> exp
val arrayExp : exp*exp -> exp
val subscriptVar : exp*exp -> exp
val fieldVar : exp*exp-> exp
val recordExp : exp list -> exp
val pp : exp -> string
end
