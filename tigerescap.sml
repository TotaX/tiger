structure tigerescap :> tigerescap =
struct
  open tigerabs
  open tigertab
  open tigerpp
  fun traverseVar (SimpleVar s) env p =
      (case tabBusca(s,env) of
         SOME (p',b) => if p > p' then b:=true else  ()
        | NONE  => raise Fail ("No existe la variable: "^s^"\n"))
   | traverseVar (FieldVar (v,sa)) env p = traverseVar v env p
   | traverseVar (SubscriptVar (v,e)) env p = (traverseVar v env p;traverseExp e
   env p)
  and
    traverseExp (CallExp ({func,args},nl)) env p = traverseExp (SeqExp (args,nl)) env p
        | traverseExp (VarExp(var,_)) env p = traverseVar var env p
        | traverseExp (OpExp ({left,oper,right},nl)) env p = (traverseExp left env p;traverseExp right env p)
        | traverseExp (RecordExp (rfields,_)) env p =  List.app (fn (s,e) => (traverseExp e env p;()) ) (#fields(rfields))
        | traverseExp (SeqExp (le,nl)) env p = List.app (fn l => traverseExp l env p ) le
        | traverseExp (AssignExp ({exp,var,...},nl)) env p = (traverseVar var env p;traverseExp exp env p)
        | traverseExp (IfExp ({test,then',else'=NONE},nl)) env p = (traverseExp test env
                p;traverseExp then' env p)
        | traverseExp (IfExp ({test,then',else'=SOME e},nl)) env p = (traverseExp test env p;traverseExp then' env p; traverseExp e env p)
        | traverseExp (ForExp ({var,escape,lo,hi,body},nl)) env p = 
                               (
                                   let val env' =  tabRInserta(var,(p, escape), env);
                                   in (traverseExp lo env p; traverseExp hi env p;
                                      traverseExp body env' p )
                                   end
                               )
        | traverseExp (BreakExp _ ) env p =  ()
        | traverseExp (ArrayExp ({size,init,...},nl)) env p = (traverseExp size env p;traverseExp init env p)
        | traverseExp (WhileExp ({test,body},nl)) env p = (traverseExp test env p; traverseExp body env p)
        | traverseExp (LetExp ({decs,body},nl)) env p = traverseExp body (traverseDecs decs env p) p
        | traverseExp _ env p = ()
  and traverseDecs [] env p = env
        |traverseDecs (s::t) env p = 
          let	fun aux s =
                          case s of
                          (FunctionDec l) => 
                                  let	fun aux(({name, params, result, body}, _), env) =
                                          let	fun aux1(x, e) =
                                                          tabRInserta(#name(x), (p+1, #escape(x)), e)
                                                  val env' = foldr aux1 env params
                                          in traverseExp body env' (p+1) ; env end
                                  in	foldl aux env l end
                          | (VarDec ({name, escape, typ, init}, _)) =>tabRInserta(name, (p, escape), env)
                          | TypeDec _ => env
                  val env' = aux s
	in traverseDecs t env' p end 

  (*definida en signature tigerescap.sig#I*)
  fun buscarEscapes prog = traverseExp prog (tabNueva()) 0
  (*definida en signature tigerescap.sig#F*)
 
end
