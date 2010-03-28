signature tigertemp =
sig
	(*	este modulo es el encargado de crear
		labels y temporarios frezcos en cada
		momento en que se lo solicite
	*)
	type temp
	val newTemp : unit -> temp
	val makestring : temp -> string
	val fromstring : string -> temp

	type label
	val newLabel : unit -> label
	val namedlabel : string -> label
	val label2string: label -> string
end
