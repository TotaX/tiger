structure tigerseman :> tigerseman =
struct
    open tigertypes
    open tigerenv
    open tigertab
    open tigerabs
    open tigertranslate
    open tigerutils
    open topsort
    open tigerstack

  type expty = {exp:tigertranslate.exp, ty:tigertypes.Ty }

  val base_tenv : (string,tigertypes.Ty) tigertab.Tabla = tigertab.tabInserList(
  tigertab.tabNueva(),
	[("int", tigertypes.TyINT),
	("string", tigertypes.TySTRING)
	])
  val mainLevel : tigertranslate.level = tigertranslate.outermost 
  val base_venv : (string, tigerenv.EnvEntry) tigertab.Tabla = tigertab.tabInserList(
  tigertab.tabNueva(),
    [
        ("print",FuncEntry {level=mainLevel, label="print",formals=[TySTRING], result=TyUNIT}),
        ("flush",FuncEntry {level=mainLevel, label="flush",formals=[],result=TyUNIT}),
        ("getchar",FuncEntry {level=mainLevel, label="getchar",formals=[],result=TySTRING}),
        ("ord",FuncEntry {level=mainLevel, label="ord",formals=[TySTRING],result=TyINT}),
        ("chr",FuncEntry {level=mainLevel, label="chr",formals=[TyINT],result=TySTRING}),
        ("size",FuncEntry {level=mainLevel, label="size",formals=[TySTRING],result=TyINT}),
        ("substring",FuncEntry {level=mainLevel, label="substring",formals=[TySTRING,TyINT,TyINT],result=TySTRING}),
        ("concat",FuncEntry {level=mainLevel, label="concat",formals=[TySTRING,TySTRING],result=TySTRING}),
        ("not",FuncEntry {level=mainLevel, label="not",formals=[TyINT],result=TyINT}),
        ("exit",FuncEntry {level=mainLevel, label="exit",formals=[TyINT],result=TyUNIT})
    ])

(* ********************************************************** *)
(* FUNCTION:toSort                                            *)
(* DESCRIPTION: funcion encargada de crear pares              *)
(*           ordenados de tipos para determinar si hay ciclos *)
(* RETURN: lista de pares ordenados                           *)
(* ********************************************************** *)
  fun toSort ({name, ty=NameTy t},p) = [(t,name)]
  |toSort ({name, ty=ArrayTy t},p) = [(t,name)]
  |toSort _ = []
(*  |toSort ({name,ty=RecordTy listNameTy},p) = *)
(*            List.map (fn {name=n,typ=NameTy typ,...}=>(typ,name)) listNameTy*)

(* ********************************************************** *)
(*@function: transExp
* @description: toma como entrada un arbol AST y realiza
* el chequeo de tipo y genera codigo intermedio para cada nodo
* del arbol de analisis sintactico.
* @param[in]: expr (Arbol AST)
* @param[out]: record con el codigo intermedio y el tipo de la 
* exprexion expr de entrada
*)
(* ********************************************************** *)
  fun transExp(tenv,venv,exp) =
    let fun trexp  (NilExp _) = {exp=nilExp() , ty=TyNIL}
            |trexp (UnitExp _) = {exp=unitExp() , ty=TyUNIT}
            |trexp (IntExp (i,_)) = {exp=intExp(i) , ty=TyINT}
            |trexp (StringExp (s,p) ) = {exp=stringExp(s) , ty=TySTRING}
            |trexp (VarExp (v,pos))=
                    ( case v of
                        SimpleVar s =>
                            ( case (tabBusca(s,venv)) of
                                SOME (VarEntry {ty,access,level}) =>
                                  {exp=simpleVar(access,level),ty=ty}
                                |SOME _ => error "No es un VarEntry" pos
                                |NONE => error ("variable "^s^" no se encuentra") pos
                            )
                        |SubscriptVar (var,exp') =>
                            let val {exp=expvar,ty} = trexp (VarExp (var,pos))
                                val ty' = case ty of
                                            TyARRAY (t,_) => t
                                            | _ => error ("no es arreglo") pos
                                 val {exp=expexp', ty=tyexp'} = trexp exp'
                             in
                                if tyexp' <> TyINT
                                then error "Tipo Incorrecto array" pos
                                else {
                                        (*T-B*)
                                        exp=subscriptVar(expexp',expvar),
                                        (*T-E*)
                                        ty=ty'
                                     }
                             end
                        |FieldVar (var, sym) =>
                            let val {exp=expvar,ty=tyvar} = trexp (VarExp ( var,pos))
                                val lfields = case tyvar of
                                               TyRECORD l => l
                                               |_ => error "no es un record" pos
                                val ty' = #2(hd(List.filter (fn (n,_) => n = sym) (#1(lfields))))
                                            handle Empty => error "error en tipo record" pos
                                (*T-B*)
                              fun busca [] _ = raise Fail "Campo Inexistente"
                                | busca (h::t) n = if (#1h) = sym 
                                                    then n
                                                    else busca t (n+1)
                              val lf = #1(lfields)
                                (*T-E*)
                             in {
                                (*T-B*)
                                exp=fieldVar(expvar,intExp(busca lf 0)),
                                (*T-E*)
                                ty=ty'
                                } 
                            end
                        )
            |trexp (SeqExp (explist,pos)) =
                let
                  (*se elimino el unEx porque semant no debe conocer nada
                  * de tigertree, se hace esto en el translate*)
                  val lexpty = List.map trexp explist
                  val ty = #ty(List.last(lexpty))
                    (*T-B*)
                  val s = List.map (#exp) (List.take (lexpty,(length lexpty)-1))
                  val e = #exp (List.last lexpty)
                    (*T-E*)
                in 
                  {
                    (*T-B*)
                    exp=seqExp(s,e),
                    (*T-E*)
                    ty=ty
                  } 
		        end
            |trexp (AssignExp ({var,exp},pos)) =
                let 
                  val _ = print "ASSIGN EXP\n"
                  val {exp=expvar,ty=tyvar} = trexp (VarExp (var,pos))
                    val {exp=expexp,ty=tyexp} = trexp exp
                in if tiposIguales(tyvar,tyexp)
                  then  {
                        (*T-B*)
                        exp=assignExp(expvar,expexp),
                        (*T-E*)
                        ty=TyUNIT
                        }
                  else error "Los Tipos de la asignacion no son iguales" pos
                end
            |trexp (BreakExp _) = ({exp=breakExp(),ty=TyUNIT}
                   handle EmptyStack => raise Fail "BREAK sin WHILE ni FOR")
            |trexp (IfExp ({test,then',else'},pos)) =
                let val {exp=exptest,ty=tytest} = trexp test
                    val _ = if tytest <> TyINT
                            then error "tipo de test no es correcto" pos
                            else ()
                    val {exp=expthen,ty=tythen} = trexp then'
                    val {exp=expelse,ty=tyelse} =
                        case else' of
                            SOME e => trexp e
                            |NONE => {exp=unitExp(),ty=TyUNIT}
                in
                  if (else' <> NONE) 
                  then
                    if tiposIguales'(tythen,tyelse) 
                        then {exp=ifExpEx(exptest,expthen,expelse),ty=tyelse}
                        else error "tipos de then y else distintos" pos
                  else
                    if tiposIguales'(tythen,TyUNIT)
                    then {exp=ifExpNx(exptest,expthen),ty=TyUNIT}
                    else error "el IF no puede devolver valor" pos
                end
            (*
            CallExp:
                1- busco en el entorno venv si existe la funcion "func":RECORD
                2- determino el tipo de los argumentos "args":LIST
                3- apareo (:P) los formales con los tipos del argumento:LIST
                4- chequeo si existe algun tipo que sea distinto
                5- si hay un tipo distinto error cc ok
            *)
            |trexp(CallExp ({func,args},pos)) =
                let val {formals,result} =
                        (case tabBusca(func,venv) of
                            SOME (FuncEntry {formals,result,...}) =>
                            {formals=formals,result=result}
                            |NONE => error (func^" no existe") pos
                            |_ => error (func^"no es una funcion") pos
                        )
                    val tyargslist = List.map (#ty o trexp) args
                    val formalArglist = ListPair.zip(tyargslist,formals)
                    val _ = if length(args) <> length (formals)
                                then error ("Cant Argumentos Incorrectos:ARGS="^makestring(length(formals))) pos
                                else ()
                    val errTyFunc = List.exists (fn (tyArgs,tyFormals)
                                                => not(tiposIguales(tyArgs,tyFormals))) formalArglist
                in
                    if errTyFunc then error "tipo incorrecto" pos
                                 else {
                                        (*T-B*)
                                        exp=SCAF,
                                        (*T-E*)
                                        ty=result
                                      }
                end
            (*
                1- determinar los tipos de "lo" , "hi" y "body"
                2- creo un nuevo entorno agregando a la variable "var"
                3- chequeamos los tipos de "lo" "hi" "body"
                    a- "body" tiene que ser de tipo unit
                    b- "lo" "hi" tienen que ser te tipo int
                4- agregamos la variable al entorno
                5- luego de agregada llamamos a transExp con el nuevo entorno
                6- buscamos el tipo de var
            *)
            |trexp (ForExp ({var,escape,lo,hi,body},pos)) =
                let
                  val {exp=explo,ty=tylo} = trexp lo
                  val {exp=exphi,ty=tyhi} = trexp hi
                  val venv' = tabRInserta(var,
                                            VarEntry {ty=tylo,level=0,
                                                    access=(allocLocal
                                                    (getTopLevel())
                                                    (!escape))}
                                         ,venv)
                  (*T-B*)
                  val _ = preForWhile() 
                  (*T-E*)
                  val {exp=expbody,ty=tybody} = transExp (tenv,venv',body)
                  val {exp=expvar,ty=tyvar} = transExp (tenv,venv',VarExp (SimpleVar var,pos))
                in
                    if tylo <> TyINT
                                then error "el tipo de LO no es int" pos
                                else
                    if tyhi <> TyINT
                                then error "el tipo de HI no es int" pos
                                else
                    if tybody <> TyUNIT
                                then error "el tipo de BODY no es unit" pos
                                else {
                                        (*T-B*)
                                        exp=forExp(expvar,explo,exphi,expbody),
                                        (*T-E*)
                                        ty=TyUNIT
                                     } 
                                        (*T-B*)
                                before posForWhile()
                                        (*T-E*)
                end
            |trexp (WhileExp ({test,body},pos)) =
                let val {exp=exptest,ty=tytest} = trexp test
                    (*T-B*)
                    val _ = preForWhile()
                    (*T-E*)
                    val {exp=expbody,ty=tybody} = trexp body
                in
                    if tytest <> TyINT
                        then error "test del while invalido debe ser entero" pos
                        else
                        if tybody <> TyUNIT
                            then error "BODY no es de tipo TyUNIT" pos
                            else {
                    (*T-B*)
                                  exp=whileExp(exptest,expbody),
                    (*T-E*)
                                  ty=TyUNIT
                                 }
                    (*T-B*)
                                before posForWhile()
                    (*T-E*)

                end
            (* array[size] of type
                1- obtenemos los tipos de "size" e "init"
                2- obtenemos del "tenv" el tipo del arreglo
                3- comparamos si el tipo del arreglo es el mismo que el de "init"
                4- comprobamos si el tipo de "size" es TyINT
            *)
            |trexp (ArrayExp ({typ,size,init},pos)) =
                let
                    val {exp=expsize,ty=tysize} = trexp size
                    val {exp=expinit,ty=tyinit} = trexp init
                    val (tyArray,uniq) = case tabBusca(typ,tenv) of
                                SOME (TyARRAY (t,u)) => (t,u)
                                |SOME p => error ("variable "^typ^" no es arreglo") pos
                                |NONE => error ("arreglo :"^typ^": no definido") pos
                in
                    if not (tiposIguales(tyArray, tyinit))
                        then error "tipo de inicializacion del arreglo" pos
                        else
                    if tysize <> TyINT
                        then error "el tamaño del arreglo debe ser entero" pos
                        else {
                                (*T-B*)
                                exp=arrayExp(expinit,expsize),
                                (*T-E*)
                                ty=TyARRAY(tyArray,uniq)
                             }
                end
            |trexp(OpExp({left, oper, right}, pos)) =
             let
                    val {exp=expleft, ty=tyleft} = trexp left
                    val {exp=expright, ty=tyright} = trexp right
                in
                    case oper of
                      (*T-B*)
                        PlusOp   => (checkInt(tyleft,tyright,pos);
                                    {       exp=buildBinOp(expleft,expright,oper),
                                            ty=TyINT
                                    })
                     |MinusOp  => (checkInt(tyleft,tyright,pos);
                                    {       exp=buildBinOp(expleft,expright,oper),
                                            ty=TyINT
                                    })
                     |TimesOp  => (checkInt(tyleft,tyright,pos);
                                    {       exp=buildBinOp(expleft,expright,oper),
                                            ty=TyINT
                                    })
                     |DivideOp => (checkInt(tyleft,tyright,pos);
                                    {       exp=buildBinOp(expleft,expright,oper),
                                            ty=TyINT
                                    })
                      (*T-E*)
                      (*T-B*)
                     |EqOp     => (if sameType(tyleft, tyright) 
                                        then {
                                               exp=buildCompOp(expleft,expright,oper),
                                               ty=TyINT
                                             }
                                       else error "tipos no concuerdan" pos)
                     |NeqOp    => (if sameType(tyleft, tyright) 
                                        then {
                                                exp=buildCompOp(expleft,expright,oper),
                                                ty=TyINT
                                             }
                                       else error "tipos no concuerdan" pos)
                     |LtOp     => (checkComp(tyleft, tyright, pos);
                                        {
                                        exp=buildCompOp(expleft,expright,oper),
                                        ty=TyINT
                                        }
                                  )
                     |LeOp     => (checkComp(tyleft, tyright, pos);
                                        {
                                        exp=buildCompOp(expleft,expright,oper),
                                        ty=TyINT
                                        }
                                  )
                     |GtOp     => (checkComp(tyleft, tyright, pos);
                                        {
                                        exp=buildCompOp(expleft,expright,oper),
                                        ty=TyINT
                                        }
                                  )
                     |GeOp     => (checkComp(tyleft, tyright, pos);
                                        {
                                        exp=buildCompOp(expleft,expright,oper),
                                        ty=TyINT
                                        }
                                  )
                     (*T-E*)
            end
          |trexp(LetExp({decs, body}, _)) =
              let
(*T-B*)
                  fun f ((t,v,ls), d) =
                    let 
                      val (v',t',ls')= transDecs ((t,v),d)
                    in 
                      (v',t',ls @ ls') 
                    end
                  val (tenv',venv',ls) = List.foldl (flip f) (tenv,venv,[]) decs
(*T-E*)
                val {exp=bodyexp,ty=tybody}=transExp (tenv',venv',body)
              in
                      {
                      (*T-B*)
                        exp=letExp(ls,bodyexp),
                      (*T-E*)  
                        ty=tybody
                      }
              end
           |trexp (RecordExp ({fields,typ},pos)) =
               let val (symTyList_typ,u) = (case tabBusca(typ,tenv) of
                               SOME (TyRECORD l) => l
                               |SOME _ => error "no es un tipo record" pos
                               |NONE => error ("no existe la variable de tipo"^typ) pos)
                   (*T-B*)
                   (*function: f
                   * description: ordena la lista fields
                   * para que tenga el mismo orden que la 
                   * lista de tipos
                   * return: lista de expty ordenada sin trexp
                   *)
                   fun f l2 = List.foldr op@ [] 
                                      (List.map
                                          (fn s => List.filter (fn (s',_) => s=s')
                                          fields)
                                          l2
                                      ) 
                   val symbolosList = List.map #1 symTyList_typ
                   val temp = f symbolosList
                   val lexpty = List.map (fn (_,e) => trexp e) temp
                   val lExp = List.map (#exp) lexpty
                   (*T-E*)
                   val symTyList_fields =  List.map (fn (s,e) => (s,#ty (trexp e))) fields

                   val fSortList = Listsort.sort (fn ((a,b),(a',b')) => String.compare (a,a'))

                   val (symList_typ,tyList_type) = ListPair.unzip (fSortList symTyList_typ)
                   val (symList_fields,tyList_fields) = ListPair.unzip (fSortList symTyList_fields)

                   val areEqListTy = if symList_typ = symList_fields
                                            andalso (ListPair.all tiposIguales (tyList_type,tyList_fields))
                                            then true else false
               in
                     if areEqListTy 
                     then   {
                            (*T-B*)
                            exp=recordExp(lExp),
                            (*T-E*)
                            ty=TyRECORD (symTyList_typ,u)
                            }
                                    else error "en algun valor del record" pos
               end
           |trexp (MinusOpExp (exp,pos)) =
                let val {exp,ty} = trexp exp
                in
                    if ty <> TyINT then error "imposible negar el tipo" pos
                        else {
                                (*T-B*)
                                exp=minusExp(exp),
                                (*T-E*)
                                ty=ty
                             }
                end
    in trexp exp  end
    and transDecs ((tenv,venv),decs) =
(*   FunctionDec  *)
(*     lo primero que hacemos es agregar todos los prototipos de las funciones  *)
(*      al entorno para que no de que una funcion no esta definida              *)
    let fun trdecs (FunctionDec []) = (tenv,venv,[])
        |trdecs (FunctionDec (f::lFuncs)) =
            let
              (*T-B*)
              val _ = incrLevel()
              (*T-E*)

                val result_ty = case #result(#1f) of
                                SOME rt => (case tabBusca(rt,tenv) of
                                             SOME t => t
                                            |NONE => raise Fail "tipo de retorno no encontrado")
                                |NONE => TyUNIT

                fun transparam{name,typ,escape} =
                     (case typ of
                        NameTy sym => (case tabBusca(sym,tenv) of
                                            SOME t => t
                                            |NONE => raise Fail ("no existe tipo " ^sym))
                        |_ => raise Fail "error interno")

                fun transparamVars({name,typ,escape},v) =
                     (case typ of
                        NameTy sym => (case tabBusca(sym,tenv) of
                                            SOME t => 
                                                (tabInserta(name,
                                                VarEntry {ty=t,
                                                            (*T-B*)
                                                            level=getLevel(),
                                                            access=allocLocal
                                                            (getTopLevel())
                                                            (!escape)
                                                            (*T-E*)
                                                        },v);())
                                            |NONE => raise Fail ("no existe tipo " ^sym))
                        |_ => raise Fail "error interno")

                val params' = List.map transparam (#params(#1f))
                val lBargs = true::List.map (fn param => !(#escape param)) (#params(#1f))
                val (tenv',venv',l) = transDecs((tenv,
                                              tabInserta(#name(#1f),
                                                FuncEntry {
                                                           formals=params',
                                                           result=result_ty,
                                                           level=newLevel((#name(#1f)),lBargs),
                                                           label=(#name(#1f))
                                                           },
                                                          venv)
                                               )
                                            ,FunctionDec lFuncs)
                val venvtemp = fromTab venv'
                val _ = List.app (fn x => transparamVars(x,venvtemp)) (#params(#1f))
                val {exp=expbody,ty=tybody} =  transExp(tenv',venvtemp,#body(#1f))
                val _ = if not (tiposIguales'(result_ty,tybody))
                            then raise Fail ("tipo de retorno de la funcion: "^(#name(#1f))^" :distintos")
                            else ()
        in
                (tenv',venv',[])
        end
        (* var a := exp *)
        |trdecs (VarDec ({name,escape,typ=NONE,init},pos)) =
            let val {exp=initExp,ty=tyInit} = transExp(tenv,venv,init)
                val acc = allocLocal (getTopLevel()) (!escape)
                val  venv' = tabInserta(name,VarEntry {
                                                        ty=tyInit,
                                                          level=getLevel(),
                                                        access=acc
                                                      },
                                     venv)
              val {exp=varExp,ty} = transExp (tenv,venv', (VarExp ((SimpleVar
              name),pos)))
            in
                (tenv ,venv',[varDec(varExp,initExp)])
            end
        (* var a:typ:= exp *)
        |trdecs (VarDec ({name,escape,typ=SOME t,init},pos)) =
            let
              val {exp=initExp,ty=tyInit} = transExp(tenv,venv,init)
              val tyTyp = case tabBusca(t,tenv) of
                            SOME x => x
                            |NONE => error ("debe definir el tipo:"^t^":") pos
              val venv' = tabInserta(name,
                                    VarEntry { ty=tyTyp,
(*T-BEGIN*)
                                               level=getLevel(),
                                               access=allocLocal
                                                         (getTopLevel())
                                                         (!escape)
(*T-END*)
                                             },
                                       venv)
(*T-BEGIN*)
              val {exp=varExp,ty} = 
                transExp (tenv,venv', (VarExp ((SimpleVar name),pos)))
(*T-END*)
            in
              if tiposIguales(tyInit,tyTyp)
                 then ( 
                        tenv,
                        venv',
(*T-BEGIN*)
                        [varDec(varExp,initExp)]
(*T-END*)
                      )
                 else error "los tipos no son iguales" pos
            end
        |trdecs (TypeDec []) = (tenv , venv,[])
        |trdecs (TypeDec (lTypes)) =
        let
            val tenv' = fromTab tenv 
(* ********************************************************** *)
(* FUNCTION: checkAndInsertTypes                              *)
(* DESCRIPTION: Chequea si se encuentra en el entorno el tipo *)
(*              si no lo encuentra lo agrega                  *)
(* RETURN: ()                                                 *)
(* ********************************************************** *)
            fun checkAndInsertTypes nameTyp typ tenv =
                        case tabBusca(nameTyp,tenv) of
                            SOME t => tabRInserta(nameTyp,typ,tenv)
                            |NONE => tabInserta(nameTyp,typ,tenv)
(* ********************************************************** *)
(* FUNCTION: trDecs                                           *)
(* DESCRIPTION: agrega los prototipos de los typos al entorno *)
(* RETURN: (), modifica directamente el entorno generado      *)
(* ********************************************************** *)
            fun trDecs ({name,ty=NameTy s},pos) = checkAndInsertTypes name (TyNAME (s,ref NONE)) tenv'
                |trDecs ({name,ty=ArrayTy s},pos) =
                    checkAndInsertTypes name (TyARRAY (TyNAME (s,ref NONE),ref ())) tenv'
                |trDecs ({name,ty=RecordTy lField},pos) =
                    let
                        val lfield = List.map (fn {name,escape,typ} =>
                                                    (case typ of
                                                        NameTy s => (name,TyNAME (s,ref NONE))
                                                        |_ => raise Fail "error interno en el record"
                                                    )
                                                ) lField
                        val _ = if tigerutils.checkList (List.map #1 lfield)
                                    then raise Fail ("hay campos repetidos en el record:"^name^":verificar")
                                    else ()
                    in
                            checkAndInsertTypes name (TyRECORD (lfield,ref ())) tenv'
                    end

            val _ = List.map trDecs lTypes
(* ********************************************************** *)
(* FUNCTION: flatten                                          *)
(* DESCRIPTION: para de lista de lista a listas               *)
(* RETURN: lista                                              *)
(* ********************************************************** *)
            val flatten = List.foldr op@ []
            val ll =  flatten (List.map toSort lTypes)

(*            val _ = (print "[";List.app (fn (a,b) => print ("("^a^","^b^")")) ll;*)
(*                    print "]\n")*)

            val lSorted = topsort ll

(*            val _ = List.map (fn x => (print x;print "->")) lSorted*)
(*            val _ = print "\n"*)

(* ********************************************************** *)
(* FUNCTION: lookupTyp                                        *)
(* DESCRIPTION: busca el tipo de un tyName apartir de la lista*)
(*              devuelta luego de hacer un topsort o la orig  *)
(* RETURN: TY si es que se encuentra alguno caso contrario err*)
(* ********************************************************** *)
           fun lookupTyp typ =
                (case tabBusca(typ,tenv') of
                    SOME t => t
                    |NONE => (case tabBusca(typ,tenv) of
                                SOME t => t
                                |NONE => raise Fail ("no se encuentra el tipo:"^typ^":declarado")
                              )
                )
(* ********************************************************** *)
(* FUNCTION: fromStrToType                                    *)
(* DESCRIPTION: esta funcion es la encargada de tomar un str  *)
(* proveniente del resultao de aplicar el topsort a cada elem *)
(* de esta lista y tipar esa string                           *)
(* RETURN: tigertypes.Ty                                      *)
(* ********************************************************** *)
            fun fromStrToType nameName typFinded (TyNAME (n,t)) = if nameName=n then typFinded else TyNAME (n,t)
                |fromStrToType arrayName typFinded (TyARRAY (t,u)) = TyARRAY (fromStrToType arrayName typFinded t,u)
                |fromStrToType s t (TyRECORD (l,u)) =
                        TyRECORD (List.map (fn (i,ty) => (i,fromStrToType s t ty)) l,u)
                |fromStrToType _ _ t = t


            val tenv'' = fromTab tenv
	    val ltenv' = tabAList tenv'
	    val _ = List.map (fn (a,b) => tabRInserta (a,b,tenv'')) ltenv'

	    fun aplica t f =
            let val lt = tabAList t
                val _ = List.map (fn (a,b) => tabRInserta (a,f b,t)) lt
            in () end

	    val _ = List.map (fn str => aplica tenv' (fromStrToType str (lookupTyp str)))
						    lSorted

	    fun rectypes (TyNAME (s,r)) = 
                                (case (tabBusca (s,tenv')) of
                                  SOME t => TyNAME(s,ref(SOME t))
				  |NONE => raise Fail "error interno!")
              | rectypes (TyARRAY (name,u)) = TyARRAY (rectypes name,u)
              | rectypes (TyRECORD (lfield,u)) =
                      let
  (*                  val _ = tigertypes.typesPP (TyRECORD (lfield,u))*)
                         val ls' = List.map (fn (s,t) => (s,rectypes t)) lfield
                      in
                          TyRECORD (ls',u)
                     end
               (* TyINT,TySTRING,TyNIL,TyUNIT, (tipos )*)
              | rectypes t = t
	    val _ = aplica tenv' rectypes
            (****************************************)
            (* tenv_nuevo = tenv_anterior +U+ tenv' *)
            (****************************************)
	    val tenv'' = fromTab tenv
	    val listTenv' = tabAList tenv'
          (* val _ = print "-----------listTenv' -------------\n"*)
          (* val _ = List.map (fn (a,b)=>(print
                    (":"^a^":");tigertypes.typesPP b)) listTenv'*)
        (*  val _ = print "----------INSERTA--------\n" *)
	    val _ = List.map (fn (a,b) => tabRInserta (a,b,tenv'')) listTenv'
        (*            
          val _ = print "----------RESULT TENV''--------\n" 
	  val _ = List.map (fn (a,b)=>(print
                    (":"^a^":");tigertypes.typesPP b)) (tabAList tenv'')
        *)
        in
         (tenv'' , venv,[])
        end
    in 
      trdecs decs 
    end

fun transProg ex =
	let	
	val main = 
          LetExp({decs=[FunctionDec [({name="_tigermain",
                                       params=[],
                                       result=SOME "int",
                                       body=ex}, 0)
                                     ]
                        ], body=UnitExp 0}, 0)
(*          val {exp,ty} = transExp(base_tenv, base_venv,main) *)
          val {exp,ty} = transExp(base_tenv, base_venv,ex) 
 in
   {exp=exp,ty=ty}
 end

end

