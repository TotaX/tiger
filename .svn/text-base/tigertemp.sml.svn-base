structure tigertemp :> tigertemp =
struct
  type temp = string
  type label = string
local
  val nt = ref 0
  val nl = ref 0
in
 (***************)
 (* TEMPORARIOS *) 
 (***************)
  fun newTemp ()  = ("T"^Int.toString(!nt)) before (nt:= !nt + 1) 
  fun makestring t = t
  fun fromstring s = s
                     
 (***************)
 (*   LABELS    *)
 (***************)
  fun newLabel () = ("L"^Int.toString(!nl)) before (nl:= !nl + 1)
  fun namedlabel s = s
  fun label2string l = l
end
      
end
