open tigerlex
open tigergrm
open BasicIO Nonstdio
open tigerpp
open tigerescap
open tigerseman

fun lexstream(is: instream) =
	Lexing.createLexer(fn b => fn n => buff_input is b 0 n);
fun errParsing(lbuf) = (print("Error en parsing!("
	^(makestring(!num_linea))^
	")["^(Lexing.getLexeme lbuf)^"]\n"); raise Fail "fin!")
fun main(args) =
	let	fun arg(l, s) =
			(List.exists (fn x => x=s) l, List.filter (fn x => x<>s) l)
		val (arbol, l1)		= arg(args, "-arbol")
		val (escapes, l2)	= arg(l1, "-escapes") 
		val (ir, l3)		= arg(l2, "-ir") 
		val (canon, l4)		= arg(l3, "-canon") 
		val (code, l5)		= arg(l4, "-code") 
		val (flow, l6)		= arg(l5, "-flow") 
		val (inter, l7)		= arg(l6, "-inter")
		val (seman,l8)      = arg(l7, "-seman") 
		val entrada =
			case l8 of
			[n] => ((open_in n)
					handle _ => raise Fail (n^" no existe!"))
			| [] => std_in
			| _ => raise Fail "opcio'n dsconocida!"
		val lexbuf = lexstream entrada
		val expr = prog Tok lexbuf handle _ => errParsing lexbuf
	in	
                (* Buscamos variables escapadas *)
                if escapes then buscarEscapes expr else ();

                (* IR: Se hace el SEMANT y el IR al mismo tiempo *)
                if ir then 
                  let
                    val {exp,...} = transProg expr
                  in
                    print (tigertranslate.pp exp)
                  end
                else ();
                
                (* Imprimimos arbol AST *)
                if arbol then exprAst expr else ();
                print "YES!!\n"
	end	handle Fail s => print("Fail: "^s^"\n")

val _ = main(CommandLine.arguments())
