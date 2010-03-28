structure tigertypes = 
struct

  type unique = unit ref (* para comparar por unit (magia negra!) *)

  datatype Ty = TyINT
              | TySTRING
              | TyRECORD of (string*Ty) list * unique
              | TyARRAY of Ty*unique
              | TyNIL
              | TyUNIT
              | TyNAME of string * Ty option ref

(* pretty Printer para los tipos *)
  fun typesPP TyINT = print "tipo=TyINT\n"
    |typesPP TySTRING = print "tipo=TySTRING\n"
    |typesPP (TyRECORD (list,_)) = (print "tipo=TyRECORD={";
                                    List.app (fn (s,tt) => (print ("id:"^s^" ty:" );typesPP tt ;print",")) list
                                    ;print"}\n")
    |typesPP (TyARRAY (t,_)) = (print "tipo=TyARRAY`[";typesPP t;print "]\n")
    |typesPP (TyNIL) = print "tipo=TyNIL\n"
    |typesPP (TyUNIT) = print "tipo=TyUNIT\n"
    |typesPP (TyNAME (s,ref(NONE))) = print ("typename="^s)
    |typesPP (TyNAME (s,ref(SOME t))) = (print ("typename="^s^" of ");
                                            typesPP t)

                                                    
end              
