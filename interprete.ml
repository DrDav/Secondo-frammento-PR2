(* Eccezioni *)
exception EmptyEnv;; (* Ambiente vuoto - quello di default *)

(* Tipi per la sintassi astratta *)
type ide = string;;
(*type 'a tuple = Void | Add of ('a * 'a tuple);;*)

(* Tipi Esprimibili *)
type eval = 
	  Int of int
	| Bool of bool
	| Tuple of tuple
	| NaN (* Not a Number, per le divisioni per 0 *)
	| Unbound 
and tuple = Void | Add of eval * tuple;;

(* Espressioni *)
type exp = 
	  Var of ide
	| Eint of int
	| Ebool of bool
	| Etuple of tuple
	| Plus of exp * exp
	| Diff of exp * exp
	| Mul of exp * exp
	| Div of exp * exp
	| Minus of exp
	| IsZero of exp
	| And of exp * exp
	| Or of exp * exp
	| Not of exp
	| Equ of exp * exp
	| LTE of exp * exp (* Less Than or Equal *)
	| GTE of exp * exp (* Greater Than or Equal *)
	| ITE of exp * exp * exp (* If Then Else *)
	| Let of ide * exp * exp
	| Fun of ide * exp
	| Appl of exp * exp
	| IsEmpty of exp
	| Slice of int * int * exp
	;;
	
type fval = FunVal of efun
and efun = exp * environment
and environment = ide -> eval;;

(* Extended Values *)
type exval = Eval of eval | Fval of fval;;

(* Ambiente / Binding *)
let env:environment = fun var -> raise EmptyEnv;;

let bind (var, espr, oldenv) = fun (src : ide) -> if src = var then espr else oldenv src;; (* Estensione di ambiente *)

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
	| ("lte", Int(n), Int(m))      -> if (n <= m) then Bool(true) else Bool(false)
	| ("gte", Int(n), Int(m))      -> if (n >= m) then Bool(true) else Bool(false)
	| _                            -> failwith "Unknown Primitive / Type Error";;

let rec slice start stop tup = match tup with
	  Void -> Void
	| Add(curr, tail) when start <= stop -> Add(curr, slice (start+1) stop tail)
	| _                                  -> Void;;
	
let eval_to_exval x = Eval(x);; (* eval -> exval *)
let fval_to_exval x = Fval(x);; (* fval -> exval *)
let parse_eval x:exval -> match x with 
	  Eval(y) -> y (* exval -> eval *)
	| _       -> failwith "Not an eval type";;
let parse_fval x:exval -> match x with
	  Fval(y) -> y (* exval -> fval *)
	| _       -> failwith "Not a fval type";;

(* Semantica Operazionale / Eseguibile *)
let rec sem (espr, amb) = match espr with
	(* Tipi primitivi + valori nell'ambiente *)
	  Var(var)                   -> eval_to_exval amb var
	| Eint(x)                    -> eval_to_exval Int x
	| Ebool(x)                   -> eval_to_exval Bool x
	| Fun(par_form, body)        -> fval_to_exval Funval(Fun(par_form, body), amb) (* chiusura *)
	(* Operazioni Primitive *)
	| Plus(add1, add2)           -> eval_to_exval op( "plus", sem (add1, amb), sem(add2, amb) )
	| Diff(min1, min2)           -> eval_to_exval op( "diff", sem (min1, amb), sem(min2, amb) )
	| Mul(fat1, fat2)            -> eval_to_exval op( "mul", sem (fat1, amb), sem(fat2, amb) )
	| Div(div1, div2)            -> eval_to_exval op( "div", sem (div1, amb), sem(div2, amb) )
	| Minus(num)                 -> eval_to_exval op( "minus", sem(num, amb), Unbound ) (* Uso speciale della costante Unbound per il terzo parametro *)
	| IsZero(num)                -> eval_to_exval op( "cmp0", sem(num, amb), Unbound )
	| And(esp1, esp2)            -> eval_to_exval op( "and", sem(esp1, amb), sem(esp2, amb) )
	| Or(esp1, esp2)             -> eval_to_exval op( "or", sem(esp1, amb), sem(esp2, amb) )
	| Equ(esp1, esp2)            -> eval_to_exval op( "equ", sem(esp1, amb), sem(esp2, amb) )
	| Not(esp)                   -> eval_to_exval op( "not", sem(esp, amb), Unbound )
	| LTE(num1, num2)            -> eval_to_exval op( "lte", sem(num1, amb), sem(num2, amb) )
	| GTE(num1, num2)            -> eval_to_exval op( "gte", sem(num1, amb), sem(num2, amb) )
	| ITE(guardia, ramoT, ramoE) -> ( match parse_eval sem(guardia, amb) with
		  Bool(g) -> if (g = true) then sem(ramoT, amb) else sem(ramoE, amb)
		| _       -> failwith "Non-boolean guard" )
	(* Operazioni Complesse *)
	| Let(var, value, body)      -> sem(body, bind(var, sem(value, amb), amb))
	| Appl(fun_name, par_att)    -> ( match parse_fval sem(fun_name, amb) with
		  Funval(Fun(par_form, body), static_env) -> sem(body, bind(par_form, sem(par_att, amb), static_env))
		| _                                       -> failwith "Not a function" )
	| IsEmpty(t)                 -> ( match t with 
		  Etuple(tupla) -> ( match tupla with
			  Void -> Bool(true)
			| _    -> Bool(false) )
		| _             -> failwith "Can't apply IsEmpty on a non-tuple value."
		)
	| Slice(start, stop, t)      -> eval_to_exval (slice start stop t)
	;;
	
