structure tigerutils =
struct
    open tigertypes
  fun error s p = raise Fail (s^" Pos: "^makestring(p))

  fun tiposIguales (t1,t2) =
        case (t1,t2) of
                (TyARRAY (_,u1),TyARRAY (_,u2)) => u1 = u2
                |(TyRECORD (_,u1),TyRECORD (_,u2)) => u1 = u2
                |(TyRECORD _, TyNIL) => true
                |(TyNIL,TyRECORD _) => true
                |(TyINT,TyINT) => true
                |(TySTRING,TySTRING) => true
                |(TyNAME (s, t),TyRECORD _) => true
                |(TyNAME (s,t),t2') => 
                    (case !t of
                        SOME e => tiposIguales (e,t2')
                        |NONE => false
                    )
                | _ => false 
    
    fun tiposIguales' (t1,t2) =
        case (t1,t2) of
            (TyUNIT,TyUNIT) => true
            |_ => tiposIguales (t1,t2)
    fun flip f (x,y) = f (y,x)

    fun getTy(ty) =
        case ty of
            TyNAME(_, rt1) => (
                case !rt1 of
                    SOME(t1) => getTy(t1)
                  | _ => raise Fail "Tipo desconocido"
                )
          | _   => ty

     fun sameType(ty1, ty2) =
        let
            fun nTy(toptref) =
                case !toptref of
                    SOME(ty) => ty
                 |  _        => raise Fail "Tipo desconocido"
        in
            case (ty1, ty2) of
                (TyRECORD(_, u1), TyRECORD(_, u2)) => u1=u2
             |  (TyARRAY(_, u1), TyARRAY(_, u2))   => u1=u2
             |  (TyNIL, TyRECORD(_))               => true
             |  (TyRECORD(_), TyNIL)               => true
             |  (TyUNIT, _)                        => true
             |  (_, TyUNIT)                        => true
             |  (TyNAME(_, t1), _)                 => sameType(nTy(t1), ty2) 
             |  (_, TyNAME(_, t2))                 => sameType(ty1, nTy(t2)) 
             |  (ty1, ty2)                         => ty1=ty2
        end
   
    fun checkType(ty1, ty2, pos) : {exp:tigertranslate.exp,ty:tigertypes.Ty} = 
        if sameType(ty1, ty2) then 
            {exp=tigertranslate.SCAF, ty=getTy(ty1)}
        else 
            error "tipos no concuerdan" pos
  
    fun checkInt(ty1, ty2, pos) =
        (
            case getTy(ty1) of TyINT => ()
                | _ => error  "integer requerido" pos;
            case getTy(ty2) of TyINT => ()
                | _ => error "integer requerido" pos;
                true
        )
        
    fun checkComp(ty1, ty2, pos) =
        if tiposIguales(ty1, ty2) then
            if (getTy(ty1) = TyINT orelse getTy(ty1) = TySTRING) then
                true
            else
                error "tipos no son comparables" pos
        else
            error "tipos no concuerdan" pos
    
    fun cmp(x, y) = x=y
	fun mem(x, []) = false
	    |mem(x, y::l) = cmp(x, y) orelse mem(x, l)
    fun checkList [] = false
        |checkList (hd::tl) = if mem(hd,tl)
                                    then true
                                    else checkList tl

end
