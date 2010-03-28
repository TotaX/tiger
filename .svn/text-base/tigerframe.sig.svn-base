signature tigerframe =
sig
(*********************************************************)
(* ACCESS:
*       InReg: la variable se encuentra en un registro
*               temporal
*       InFrame: para cuando la variable esta en memoria
*                offset creado por allocLocal
*)
(*********************************************************)

 datatype access = InReg of tigertemp.temp (*esta en registro*)
                    |InFrame of int (*offSet*) (*esta en memoria*)

 type frame =  {
                name : tigertemp.label,
                formals : bool list,
                stackBreak : tigertemp.label tigerstack.Stack,
                offLocalActual : int ref,
                vars : (string,access) tigertab.Tabla
              }

datatype frag = PROC of tigertree.stm * frame 
                | STRING of tigertemp.label * string
val fp : tigertemp.temp
val rv : tigertemp.temp
val ov : tigertemp.temp
val wordSize : int
val newFrame : {name:tigertemp.label,formals:bool list} -> frame
(* A IMPLMENTAR *)
val name : frame -> tigertemp.label
(*val formals : frame -> access list*)
val allocLocal : frame -> bool -> access
(* A IMPLMENTAR *)
val guarda : frag -> unit 
end
