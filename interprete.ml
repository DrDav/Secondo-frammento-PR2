(* Eccezioni *)
exception EmptyEnv;; (* Ambiente vuoto - quello di default *)

(* Tipi per la sintassi astratta *)
type ide = string;;
type 'a etuple = Void | Add of ('a * etuple);;

(* Espressioni *)
type exp = 
	  Var of ide
	| Eint of int
	| Ebool of bool
	| Etuple of int etuple
	| Plus of exp * exp
	| Diff of exp * exp
	| Mul of exp * exp
	| Div of exp * exp
	| Minus of exp
	| IsZero of exp
	| And of exp * exp
	| Or of exp * exp
	| Not of exp
	| Equ of exp
	| LTE of exp * exp (* Less Than or Equal *)
	| GTE of exp * exp (* Greater Than or Equal *)
	| ITE of exp * exp * exp (* If Then Else *)
	| Let of ide * exp * exp
	| Fun of ide * exp
	| Appl of exp * exp
	| IsEmpty of exp
	;;

(* Tipi Esprimibili *)
type eval = 
	  Int of int
	| NaN
	| Bool of bool
	| Funval of efun
	| Tuple of int etuple
	| Unbound
and efun = exp * environment
and environment = ide -> eval;;

(* Ambiente / Binding *)
let env:environment = fun var -> raise EmptyEnv;;

let bind var espr oldenv = fun (src : ide) -> if src = var then espr else oldenv src;; (* Estensione di ambiente *)

(* Utility Functions *)
let op (operation, x, y) = match (operation, x, y) with
	  ("plus", Int(op1), Int(op2)) -> Int(op1+op2)
	| ("diff", Int(op1), Int(op2)) -> Int(op1-op2)
	| ("mul", Int(op1), Int(op2))  -> Int(op1*op2)
	| ("div", Int(op1), Int(op2))  -> if (op2 = 0) then NaN else Int(op1/op2)
	| ("minus", Int(num), _)       -> Int(-num)
	| ("cmp0", Int(num), _)        -> if (num = 0) then Bool(true) else Bool(false)
	| ("and", Bool(a), Bool(b))    -> if (a = false) then Bool(false) else Bool(b) (* Greedy *)
	| ("or", Bool(a), Bool(b))     -> if (a = true) then Bool(true) else Bool(b) (* Greedy *)
	| ("equ", Int(a), Int(b))      -> if (a = b) then Bool(true) else Bool(false) 
	| ("not", Bool(a), _)          -> if (a = true) then Bool(false) else Bool(true)
	| _                            -> failwith "Unknown Primitive / Type Error";;

(* Semantica Operazionale / Eseguibile *)
let rec sem (espr, amb) = match espr with
	(* Tipi primitivi + valori nell'ambiente *)
	  Var(var)                   -> amb var
	| Eint(x)                    -> Int x
	| Ebool(x)                   -> Bool x
	| Fun(par_form, body)        -> Funval(Fun(par_form, body), amb) (* chiusura *)
	(* Operazioni Primitive *)
	| Plus(add1, add2)           -> op( "plus", sem (add1, amb), sem(add2, amb) )
	| Diff(min1, min2)           -> op( "diff", sem (min1, amb), sem(min2, amb) )
	| Mul(fat1, fat2)            -> op( "mul", sem (fat1, amb), sem(fat2, amb) )
	| Div(add1, add2)            -> op( "div", sem (div1, amb), sem(div2, amb) )
	| Minus(num)                 -> op( "minus", sem(num, amb), Unbound ) (* Uso speciale della costante Unbound per il terzo parametro *)
	| IsZero(num)                -> op( "cmp0", sem(num, amb), Unbound )
	| And(esp1, esp2)            -> op( "and", sem(esp1, amb), sem(esp2, amb) )
	| Or(esp1, esp2)             -> op( "or", sem(esp1, amb), sem(esp2, amb) )
	| Equ(esp1, esp2)            -> op( "equ", sem(esp1, amb), sem(esp2, amb) )
	| Not(esp)                   -> op( "not", sem(esp, amb), Unbound )
	| ITE(guardia, ramoT, ramoE) -> ( match sem(guardia, amb) with
		  Bool(g) -> if (g = true) then sem(ramoT, amb) else sem(ramoE, amb)
		| _       -> failwith "Nonboolean guard" )
	(* Operazioni Complesse *)
	| Let(var, value, body)      -> sem(body, bind(var, sem(value, amb), amb))
	| Appl(fun_name, par_att)    -> ( match sem(fun_name, amb) with
		  Funval(Fun(par_form, body), static_env) -> sem(body, bind(par_form, sem(par_att, amb), static_env))
		| _                                       -> failwith "Not a function" )
	| IsEmpty(t)                 -> ( match t with 
		  Etuple(tupla) -> ( match tupla with
			  Void -> Bool(true)
			| _    -> Bool(false) )
		| _             -> failwith "Can't apply IsEmpty on a non-tuple value."
		)
	
	;;
	
