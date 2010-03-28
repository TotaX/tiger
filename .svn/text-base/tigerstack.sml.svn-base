structure tigerstack :> tigerstack =
struct
  type 'Elem Stack = 'Elem list ref

(**************************)
(*   exceptions     PILA  *)
(**************************)
exception EmptyStack;
(**************************)
(*   implementacion PILA  *)
(**************************)
  fun stackNew () = ref []
  fun stackIsEmpty p = null(!p)
  fun stackPush e p = (p:= e:: !p)
  fun stackPop p = if stackIsEmpty p then raise EmptyStack
                   else (hd(!p)) before (p:= tl(!p))
  fun stackTop p = if stackIsEmpty p then raise EmptyStack
                   else hd(!p)

end
