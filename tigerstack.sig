signature tigerstack =
sig

type 'Elem Stack
exception EmptyStack
  val stackNew : unit -> 'Elem Stack
  val stackPush : 'Elem -> 'Elem Stack -> unit
  val stackPop : 'Elem Stack -> 'Elem
  val stackTop : 'Elem Stack -> 'Elem
  val stackIsEmpty : 'Elem Stack -> bool
end
