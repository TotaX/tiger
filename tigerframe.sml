structure tigerframe :> tigerframe =
struct
open tigerstack
open tigertab
structure T = tigertree
structure TEMP = tigertemp

  val fp = TEMP.fromstring "%ebp"
  val rv = TEMP.fromstring "%eax"
  val ov = TEMP.fromstring "%edx" (*Intel y la ·@&!!! *)

(*********************************************************)
(*                   OFFSET                              *)
(*********************************************************)
  val wordSize = 4 (* tamaño de una palabra en bytes *)
  val offIncr = wordSize (*32 ó 64*)
  val offArgs = ref 8   (* para i386 *)
  val offLocals = ref 0 (* sin callees, i386*)
(*********************************************************)
(* ACCESS:
*       InReg: la variable se encuentra en un registro
*               temporal
*       InFrame: para cuando la variable esta en memoria
*                offset creado por allocLocal
*)
(*********************************************************)

 datatype access = InReg of TEMP.temp (*esta en registro*)
                    |InFrame of int (*offSet*) (*esta en memoria*)

                    
(*********************************************************)
(*FRAME:
*       name : nombre de la funcion / activationRecord
*       formales: lista de formales de la funcion / AR
*       stackBreak: pila de breaks que pueden estar 
*                   en los bucles while y for unicamente
*       frags: guardaremos los codigos intermedios de las 
*               funciones  y las strings Constantes
*       offLocalActual: valores auxiliares para la genera-
*                       ci'on de offSets para InFrame 
*       vars:  guardara el tipo de acceso de las varia
*               -bles (InReg/InFrame).
*)
(*********************************************************)
type frame =  {
                name : TEMP.label,
                formals : bool list,
                stackBreak : tigertemp.label tigerstack.Stack,
                offLocalActual : int ref,
                vars : (string,access) tigertab.Tabla
              }
 
(*********************************************************)
(*                     FRAGMENTOS                        *)
(*********************************************************)
  
  datatype frag = PROC of tigertree.stm * frame 
                | STRING of tigertemp.label * string

  val fraglist = ref []
  fun guarda frag = fraglist:= frag :: !fraglist

(*********************************************************)
(*                   FRAME.UTILITARIOS                   *)
(*********************************************************)
  fun newFrame{name : TEMP.label, formals : bool list} =
         {
            name = name,
            formals = formals,
            stackBreak = stackNew(),
            offLocalActual = ref ~4,
            vars = tabNueva()
         }

  fun name (f:frame) = #name(f)
  (*
  *@func: newOff
  *@param[in]: bool
  *@param[out]: int
  *@desc: crea un nuevo offset dependiendo si es un argumento
  *       o un local
  *)
  fun newOff al =
    if al then (*argumento*)
      let val off = !offArgs
      in
        offArgs := !offArgs + offIncr;
        off
      end
    else (*local*)
    let
      val off = !offLocals
    in
      offLocals := !offLocals - offIncr;
      off
    end
  fun allocLocal f b =
    case b of
         true => (*Variable escapada a memoria*) 
         let
           val inmem = InFrame (newOff true)
         in
           inmem
         end
       |false => InReg (TEMP.newTemp())

end (* FRAME *)
