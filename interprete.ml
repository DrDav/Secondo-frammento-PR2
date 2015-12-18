(* Eccezioni *)
exception EmptyEnv;; (* Ambiente vuoto - quello di default *)

(* Tipi per la sintassi astratta *)
type ide = string;;

type eval = 
	  Int of int
	| NaN
	| Bool of bool
	(*| Funval of efun*)
	| Tuple of etuple
	| Unbound
(*and efun = exp * env*)
and etuple = Void | Add of eval * etuple;;

(* Espressioni *)
type exp = 
	  Var of ide
	| Eint of int
	| Ebool of bool
	| Plus of exp * exp
	| Diff of exp * exp
	| Mul of exp * exp
	| Div of exp * exp;;

(* Ambiente / Binding *)
let env = fun (var : ide) -> raise EmptyEnv;;

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
	| _                            -> failwith "Unknown Primitive / Type Error";;
	
let rec greedy_logic_op (operation, x, y) = match (operation, x, y) with
	  ("and", Bool(a), Bool(b)) -> if(a = false) then Bool(false) else greedy_logic

(* Semantica Operazionale / Eseguibile *)
let rec sem (espr, amb) = match espr with
	  Var(var) -> amb var
	| Eint(x) -> Int x
	| Ebool(x) -> Bool x
	| Plus(add1, add2) -> op( "plus", sem (add1, amb), sem(add2, amb) )
	| Diff(min1, min2) -> op( "diff", sem (min1, amb), sem(min2, amb) )
	| Mul(fat1, fat2)  -> op( "mul", sem (fat1, amb), sem(fat2, amb) )
	| Div(add1, add2)  -> op( "div", sem (div1, amb), sem(div2, amb) )
	| Minus(num)       -> op( "minus", sem(num, amb), Unbound ) (* Uso speciale della costante Unbound per il terzo parametro *)
	| IsZero(num)      -> op( "cmp0", sem(num, amb), Unbound )
	| And(esp1, esp2)  -> op( "and", sem(esp1, amb), sem(esp2, amb) )
	| Or(esp1, esp2)   -> op( "or", sem(esp1, amb), sem(esp2, amb) )
	| Equ(esp1, esp2)  -> op( "equ", sem(esp1, amb), sem(esp2, amb) )
	| LTE(esp1, esp2)  -> op( "lessthaneq", sem(esp1, amb), sem(esp2, amb) )
	| GTE(esp1, esp2)  -> 
	;;
	
