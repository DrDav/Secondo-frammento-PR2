(* Eccezioni *)
exception EmptyEnv;; (* Ambiente vuoto - quello di default *)
exception OutOfBounds;; (* Accesso fuori dalla tupla *)
exception InvalidLoop;; (* Dichiarazione di funzione all'interno del for *)

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
	(* Espressioni per valori base *)
	  Var of ide
	| Eint of int
	| Ebool of bool
	| Etuple of tuple
	(* Operazioni binarie/unarie su interi/booleani *)
	| Plus of exp * exp
	| Diff of exp * exp
	| Mul of exp * exp
	| Div of exp * exp
	| Minus of exp
	| IsZero of exp
	| And of exp * exp
	| Or of exp * exp
	| Not of exp
	| Equ of exp * exp (* Equals *)
	| LTE of exp * exp (* Less Than or Equal *)
	| GTE of exp * exp (* Greater Than or Equal *)
	(* Operazioni complesse *)
	| ITE of exp * exp * exp (* If Then Else *)
	| Let of ide * exp * exp
	| Fun of ide * exp (* Dichiarazione di Funzione *)
	| Appl of exp * exp (* Applicazione di Funzione *)
	| IsEmpty of tuple (* Controlla se una tupla è vuota *)
	| Slice of int * int * exp (* Operatore di Slicing su tuple *)
	| Access of int * exp (* Accede direttamente all'i-esimo elemento di una tupla *)
	| In of exp * tuple (* Controlla se un certo valore è contenuto in una certa tupla *)
	| For of ide * tuple * exp (* For Loop su tuple *)
	;;
	
type fval = Funval of efun
and efun = exp * environment (* Chiusura *)
and environment = ide -> exval (* Tipo degli ambienti *)
and
(* Extended Values - per trattare funzioni e valori allo stesso modo *)
    exval = Eval of eval | Fval of fval;;

(* Ambiente / Binding *)
let env:environment = fun var -> raise EmptyEnv;; (* Ambiente base - quello vuoto *)

let bind (var, espr, (oldenv:environment)) = fun (src : ide) -> if src = var then espr else oldenv src;; (* Estensione di ambiente *)

(* Utility Functions *)
let eval_to_exval x = Eval(x);; (* eval -> exval *)
let fval_to_exval x = Fval(x);; (* fval -> exval *)
let parse_eval x = match x with (* Inverso di eval_to_exval, consente di "spacchettare" un eval da un exval in cui era impacchettato *)
	  Eval(y) -> y (* exval -> eval *)
	| _       -> failwith "Not an eval type";;
let parse_fval x = match x with (* Inverso di fval_to_exval, consente di "spacchettare" una fval da un exval in cui era impacchettato *)
	  Fval(y) -> y (* exval -> fval *)
	| _       -> failwith "Not a fval type";;

(* Restituisce una stringa che indica il tipo di un eval *)	
let get_type elem = match elem with
	  Int(_)   -> "int"
	| Bool(_)  -> "bool" 
	| Tuple(_) -> "tupla"
	| _        -> failwith "Non-primitive type";;

(* Operazioni unarie/binarie su interi/booleani: restituiscono tutte un eval *)
let op (operation, x, y) = match (operation, parse_eval x, parse_eval y) with
(* i parametri vengono parsati in eval perché dalla sem() arrivano già racchiusi in una exval *) 
	  ("plus", Int(op1), Int(op2)) -> Int(op1+op2)
	| ("diff", Int(op1), Int(op2)) -> Int(op1-op2)
	| ("mul", Int(op1), Int(op2))  -> Int(op1*op2)
	| ("div", Int(op1), Int(op2))  -> if (op2 = 0) then NaN else Int(op1/op2) 
	| ("minus", Int(num), _)       -> Int(-num)
	| ("cmp0", Int(num), _)        -> if (num = 0) then Bool(true) else Bool(false)
	| ("and", Bool(a), Bool(b))    -> if (a = false) then Bool(false) else Bool(b) (* Greedy - regola esterna *)
	| ("or", Bool(a), Bool(b))     -> if (a = true) then Bool(true) else Bool(b) (* Greedy - regola esterna *)
	| ("equ", Int(a), Int(b))      -> if (a = b) then Bool(true) else Bool(false) 
	| ("not", Bool(a), _)          -> if (a = true) then Bool(false) else Bool(true)
	| ("lte", Int(n), Int(m))      -> if (n <= m) then Bool(true) else Bool(false)
	| ("gte", Int(n), Int(m))      -> if (n >= m) then Bool(true) else Bool(false)
	| _                            -> failwith "Unknown Primitive / Type Error";;

(* Calcola la lunghezza di una tupla *)
let rec t_length t = match t with
			  Void         -> 0
			| Add(_, tail) -> 1 + t_length tail;;

(* Accesso diretto all'elemento in posizione position della tupla tup *) 	
let direct_access position tup =
	let rec direct_access_rec (position, tup, n) = (* Funzione ausiliaria ricorsiva *)
		match tup with
			  Void             -> raise OutOfBounds (* Sono arrivato fuori dalla tupla, o la tupla era vuota *)
			| Add(value, tail) -> if (n = position) then value else direct_access_rec (position, tail, (n+1))
	in 
	if (position >= 0) then direct_access_rec (position, tup, 0) else direct_access_rec ((t_length tup)+position, tup, 0);;
	(* Accedere all'elemento i-esimo con i negativo è come accedere all'elemento (length+i)-esimo (lo rendo positivo) *)
	
(* Operazione di slicing (inclusiva) su tuple *)	
let rec slice start stop tup = 
	if (start < 0 && stop <= (start+1)) then (* Indici entrambi positivi... *)
		if (start >= stop) then Add((direct_access start tup), slice (start-1) stop tup) else Void (* chiusura ricorsione *)
	else if (start >= 0 && stop >= (start-1)) then (* ... o entrambi negativi (lavoro sporco lasciato alla direct_access() *)
		if (start <= stop) then Add((direct_access start tup), slice (start+1) stop tup) else Void (* chiusura ricorsione *)
	else (* Nessun'altra combinazione consentita *)
		raise OutOfBounds;;

(* Cerca needle all'interno della tupla where *)	
let rec search (needle, where) = match where with
	  Void      -> Bool(false)
	| Add(v, t) -> if(v = parse_eval(needle)) then Bool(true) else search (needle, t);; (* ricerca lineare ricorsiva *)
	
(* Semantica Operazionale / Eseguibile *)
(* Restituisce una EXVAL (extended value) per trattare allo stesso livello funzioni e valori base (e per uniformare il tipo restituito) *)
let rec sem (espr, amb) = match espr with
	(* Tipi primitivi + valori nell'ambiente *)
	  Var(var)                   -> amb var (* l'ambiente restituisce già una exval *)
	| Eint(x)                    -> eval_to_exval( Int x )
	| Ebool(x)                   -> eval_to_exval( Bool x )
	| Etuple(x)                  -> eval_to_exval( Tuple x )
	| Fun(par_form, body)        -> fval_to_exval( Funval(Fun(par_form, body), amb) ) (* chiusura *)
	(* Operazioni Primitive *)
	| Plus(add1, add2)           -> eval_to_exval( op( "plus", sem (add1, amb), sem(add2, amb) ) )
	| Diff(min1, min2)           -> eval_to_exval( op( "diff", sem (min1, amb), sem(min2, amb) ) )
	| Mul(fat1, fat2)            -> eval_to_exval( op( "mul", sem (fat1, amb), sem(fat2, amb) ) )
	| Div(div1, div2)            -> eval_to_exval( op( "div", sem (div1, amb), sem(div2, amb) ) )
	| Minus(num)                 -> eval_to_exval( op( "minus", sem(num, amb), eval_to_exval Unbound ) ) (* Uso speciale della costante Unbound per il terzo parametro *)
	| IsZero(num)                -> eval_to_exval( op( "cmp0", sem(num, amb), eval_to_exval Unbound ) )
	| And(esp1, esp2)            -> eval_to_exval( op( "and", sem(esp1, amb), sem(esp2, amb) ) )
	| Or(esp1, esp2)             -> eval_to_exval( op( "or", sem(esp1, amb), sem(esp2, amb) ) )
	| Equ(esp1, esp2)            -> eval_to_exval( op( "equ", sem(esp1, amb), sem(esp2, amb) ) )
	| Not(esp)                   -> eval_to_exval( op( "not", sem(esp, amb), eval_to_exval Unbound ) )
	| LTE(num1, num2)            -> eval_to_exval( op( "lte", sem(num1, amb), sem(num2, amb) ) )
	| GTE(num1, num2)            -> eval_to_exval( op( "gte", sem(num1, amb), sem(num2, amb) ) )
	(* Operazioni Complesse *)
	| ITE(guardia, ramoT, ramoE) -> ( match parse_eval( sem(guardia, amb) ) with
		  Bool(g) -> if (g = true) then sem(ramoT, amb) else sem(ramoE, amb)
		| _       -> failwith "Non-boolean guard" )
	| Let(var, value, body)      -> sem(body, bind(var, sem(value, amb), amb))
	| Appl(fun_name, par_att)    -> ( match parse_fval( sem(fun_name, amb) ) with
		  Funval(Fun(par_form, body), static_env) -> sem(body, bind(par_form, sem(par_att, amb), static_env))
		| _                                       -> failwith "Not a function" )
	| IsEmpty(t)                 -> ( match t with 
		  Void -> eval_to_exval( Bool(true) )
		| _    -> eval_to_exval( Bool(false) ) 
		)
	| Slice(start, stop, tupexp) -> ( match parse_eval( sem(tupexp, amb) ) with
		  Tuple(tupla) -> eval_to_exval( Tuple(slice start stop tupla) ) 
		| _            -> failwith "Can't slice a non-tuple value."  
		)
	| Access(pos, tupexp)        -> ( match parse_eval( sem(tupexp, amb) ) with
		  Tuple(tupla) -> eval_to_exval ( direct_access pos tupla )
		| _            -> failwith "Can't access to a position on a non-tuple value" 
		)
	| In(value, tupla)           -> eval_to_exval( search( sem(value, amb), tupla ) )
	| For(var, tupla, body)      -> let rec loop tup = match tup with
		  Add(elem, tail) -> (try
					let semantica = sem(body, bind(var, (eval_to_exval elem), amb))
					in (match semantica with
						  Fval(_) -> raise InvalidLoop
						| _       -> Add( parse_eval (semantica), (* Se è del tipo giusto applico l'operazione *)
								  loop tail))
				   with
					Failure("Unknown Primitive / Type Error") ->  loop tail )(* Se non è del tipo inziale vado avanti *)
		| _		  -> Void
					in eval_to_exval(Tuple(loop tupla))
	| _                          -> failwith "Non legal expression"
	;;
	
(* Funzioni utili per valutare espressioni senza appesantire la notazione *) 
let ev environment = fun expression -> parse_eval( sem( expression, environment ) );;
let evaluate = ev env;; (* valuta nell'ambiente di default *)
let get_tuple eval = match eval with (* Restituisce la tupla di tipo tuple, partendo dal tipo eval *)
	  Tuple(t) -> t
	| _        -> Void;;
	
let toList tupla = (* trasforma una tupla in lista, per una leggerla meglio *)
	let rec toL t = match t with
		  Void            -> []
		| Add(elem, tail) -> match elem with 
			  Tuple(tupla_interna) -> (toL tupla_interna) @ toL tail
			| _                    -> elem :: toL tail
	in toL (get_tuple tupla);;
	
	
(* Tupla dell'esempio - 23, true, (45, 7), false*)

let tupexample = Etuple( Add(Int 23, Add(Bool true, Add(Tuple( Add(Int 45, Add(Int 7, Void)) ), Add(Bool false, Add(Int 12, Void ) ) ) ) ) );;
let ciclo = For("x", get_tuple( evaluate tupexample ), Plus(Var "x", Eint(1)));;

evaluate (Access(0, tupexample));;     (* t[0] -> Int 23 *)
toList (evaluate (Access(2, tupexample)));;     (* t[2] -> [Int 45; Int 7] *)
evaluate (Access(4, tupexample));;     (* t[4] -> eccezione *)
evaluate (Access(-1, tupexample));;    (* t[-1] -> Bool false *)
evaluate (Access(-3, tupexample));;    (* t[-3] -> Bool true *)
evaluate (Access(-5, tupexample));;    (* t[-5] -> eccezione *)
toList (evaluate (Slice(1, 2, tupexample)));;   (* t[1:2] -> (Bool true, (Int 45, Int 7)) *)
toList (evaluate (Slice(-1, -3, tupexample)));; (* t[-1:-3] -> (Bool false, (Int 45, Int 7), Bool true) *)
toList (evaluate (Slice(-1, 1, tupexample)));;  (* t[-1:1] -> eccezione *)
toList (evaluate ciclo);;              (* [24] *)
