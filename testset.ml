(* (23, true, (45, 7), false, 12)*)
let tup1 = Add(Int 23, Add(Bool true, Add(Tuple( Add(Int 45, Add(Int 7, Void)) ), Add(Bool false, Add(Int 12, Void ) ) ) ) );;
let ciclo = For("x", tup1, Plus(Var "x", Eint(1)));;

(* Casi di test sulla tupla tup1 *)
evaluate (Access(0, Etuple(tup1)));;              (* t[0] -> Int 23 *)
toList (evaluate (Access(2, Etuple(tup1))));;     (* t[2] -> [Int 45; Int 7] *)
evaluate (Access(4, Etuple(tup1)));;              (* t[4] -> eccezione *)
evaluate (Access(-1, Etuple(tup1)));;             (* t[-1] -> Bool false *)
evaluate (Access(-3, Etuple(tup1)));;             (* t[-3] -> Bool true *)
evaluate (Access(-5, Etuple(tup1)));;             (* t[-5] -> eccezione *)
toList (evaluate (Slice(1, 2, Etuple(tup1))));;   (* t[1:2] -> (Bool true, (Int 45, Int 7)) *)
toList (evaluate (Slice(-1, -3, Etuple(tup1))));; (* t[-1:-3] -> (Bool false, (Int 45, Int 7), Bool true) *)
toList (evaluate (Slice(-1, 1, Etuple(tup1))));;  (* t[-1:1] -> eccezione *)
toList (evaluate ciclo);;                         (* [24, 13] *)
evaluate (In(Eint 45, tup1));;                    (* False, perché 45 è in una sottotupla *)
evaluate (In(Eint 12, tup1));;                    (* True, 12 è l'ultimo valore della tupla *)
evaluate (In(Etuple(Add(Int 45, Add(Int 7, Void))), tup1);; (* True, è la sottotupla *)

(* (false, true, false, true) *)
let tuponlybool = Add(Bool false, Add(Bool true, Add(Bool false, Add(Bool true, Void ) ) ) );;
(* (-1, 33, -8, -92, -3, 15) *)
let tuponlynum = Add(Int (-1), Add(Int 33, Add(Int (-8), Add(Int (-92), Add(Int (-3), Add(Int 15, Void) ) ) ) ) );;

toList (evaluate(For("val", tuponlybool, Not(Var "val"))));; (* [true, false, true, false] *)

(* Calcola il resto della divisione per 2 di un numero *)
let restoper2 = Fun("n", Diff(Var "n", Mul(Div(Var "n", Eint 2), Eint 2)));;
(* Restituisce true se n è pari, false altrimenti *)
let true_if_even = Fun("n", ITE( Equ( Appl(restoper2, Var "n"), Eint(0) ), Ebool(true), Ebool(false)));;

(* Applica true_if_even a tutti gli elementi della tupla tuponlynum *) 
toList (evaluate (For("n", tuponlynum, Appl(true_if_even, Var "n"))));; (* [false, false, true, true, false, false] *)
(* Applica true_if_even alla sottotupla tuponlynum[-1:-4] (che sarebbe (-15, -3, -92, -8) )*)
toList (evaluate (For("elem", get_tuple(evaluate (Slice(-1, -4, Etuple(tuponlynum)))), Appl(true_if_even, Var "elem"))));; (* [false, false, true, true] *)
(* Restituisce una tupla con i resti della divisione per 2 di ogni elemento *)
toList (evaluate (For("n", tuponlynum, Appl(restoper2, Var "n"))));; (* [-1, 1, 0, 0, -1, 1] *)

(* Funzione che calcola il valore assoluto di un numero *)
let abs = Fun("num", ITE( And( GTE(Var "num", Eint 0), Ebool(true) ), Var "num", Minus(Var "num")));;
toList (evaluate (For("x", tuponlynum, Appl(abs, Var "x"))));; (* [1, 33, 8, 92, 3, 15] *)
(* Cambia il segno di tutti gli interi della tupla 'tuponlynum' *)
toList (evaluate (For("x", tuponlynum, Minus(Var "x"))));; (* [1, -33, 8, 92, 3, -15] *)

(* Esegue lo XOR logico tra il primo booleano della tupla tuponlybool e gli altri valori *)
let xor = For("z", tuponlybool, Let("y", Access(0, Etuple(tuponlybool)), Appl(Fun("x", And(Or(Var "x", Var "y"), Not(And(Var "x", Var "y")))), Var "z")));
toList (evaluate (xor));; (* [false, true, false, true] *)

(* Esempio di scoping statico *)
(* let y = 10 in 
	let funz = fun x -> x*y in 
		let y = 0 in 
			funz 2
*)
evaluate (Let("y", Eint 10, Let("funz", Fun("x", Mul(Var "x", Var "y")), Let("y", Eint 0, Appl(Var "funz", Eint 2)))));; (* 20 *)
(* Se lo scoping fosse stato dinamico il risultato sarebbe stato 0 *)

(* ( (), (), (3, false) ) *)
let tupladituple = Add(Tuple(Void), Add(Tuple(Void), Add(Tuple(Add(Int 3, Add(Bool false, Void))), Void)));; 

evaluate (IsEmpty(tupladituple));; (* false *)
evaluate (IsEmpty(get_tuple(evaluate (Access(1, Etuple(tupladituple))))));; (* True, il primo elemento è la tupla vuota *)
evaluate (IsEmpty(get_tuple(evaluate (Access(-1, Etuple(tupladituple))))));; (* false, l'ultimo elemento è la tupla (3, false) *)

let last_tupla = Add(Int 0, Add(Int 2, Add(Bool true, Add(Int 0, Add(Bool false, Add(Int 0, Add(Int 12, Void)))))));
For("elem", last_tupla, IsZero(Var "elem"));; (* [true, false, true, true, false] - i valori non numerici vengono ignorati *)
