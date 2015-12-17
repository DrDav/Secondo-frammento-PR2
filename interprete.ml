(* Eccezioni *)
exception EmptyEnv;; (* Ambiente vuoto - quello di default *)

(* Tipi per la sintassi astratta *)
type ide = string;;

type eval = 
	  Int of int
	| Bool of bool
	(*| Funval of efun*)
	| Tuple of etuple
	| Unbound
(*and efun = exp * env*)
and etuple = Void | Add of eval * etuple;;

(* Espressioni *)
type exp = Var of ide | Eint of int | Ebool of bool | Plus of exp * exp;;

(* Ambiente / Binding *)
let env : exp = fun (var : ide) -> raise EmptyEnv;;

let bind var espr oldenv = fun (src : ide) -> if src = var then espr else oldenv src;; (* Estensione di ambiente *)

(* Semantica Operazionale / Eseguibile *)
let rec sem espr amb = match espr with
	  Var(var) -> amb var
	| Eint(x) -> Int x
	| Ebool(x) -> Bool x
	| Plus(add1, add2) -> Int(sem(add1, amb) + sem(add2,amb));;
	
