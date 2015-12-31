(* (23, true, (45, 7), false, 12)*)
let tup1 = Add(Int 23, Add(Bool true, Add(Tuple( Add(Int 45, Add(Int 7, Void)) ), Add(Bool false, Add(Int 12, Void ) ) ) ) );;
let ciclo = For("x", tup1, Plus(Var "x", Eint(1)));;

evaluate (Access(0, Etuple(tup1)));;     (* t[0] -> Int 23 *)
toList (evaluate (Access(2, Etuple(tup1))));;     (* t[2] -> [Int 45; Int 7] *)
evaluate (Access(4, Etuple(tup1)));;     (* t[4] -> eccezione *)
evaluate (Access(-1, Etuple(tup1)));;    (* t[-1] -> Bool false *)
evaluate (Access(-3, Etuple(tup1)));;    (* t[-3] -> Bool true *)
evaluate (Access(-5, Etuple(tup1)));;    (* t[-5] -> eccezione *)
toList (evaluate (Slice(1, 2, Etuple(tup1))));;   (* t[1:2] -> (Bool true, (Int 45, Int 7)) *)
toList (evaluate (Slice(-1, -3, Etuple(tup1))));; (* t[-1:-3] -> (Bool false, (Int 45, Int 7), Bool true) *)
toList (evaluate (Slice(-1, 1, Etuple(tup1))));;  (* t[-1:1] -> eccezione *)
toList (evaluate ciclo);;              (* [24] *)

let tup2 = Add(Int 23, Add(Bool true, Add(Tuple( Add(Int 45, Add(Int 7, Void)) ), Add(Bool false, Add(Int 12, Void ) ) ) ) );;
let tuponlynum = Add(Int (-1), Add(Int 33, Add(Int (-8), Add(Int (-92), Add(Int (-3), Add(Int 15, Void) ) ) ) ) );;

toList (evaluate(For("val", tup2, Not(Var "val"))));;

(* Calcola il resto della divisione per 2 di un numero *)
let restoper2 = Fun("n", Diff(Var "n", Mul(Div(Var "n", Eint 2), Eint 2)));;
(* Restituisce true se n è pari, false altrimenti *)
let true_if_even = Fun("n", ITE( Equ( Appl(restoper2, Var "n"), Eint(0) ), Ebool(true), Ebool(false)));;

(* Applica true_if_even a tutti gli elementi della tupla tupexample *) 
toList (evaluate (For("n", tup2, Appl(true_if_even, Var "n"))));;
toList (evaluate (For("n", tup2, Appl(restoper2, Var "n"))));;

(* Somma il primo elemento di una tupla a tutti gli elementi della tupla stessa *)
let sommatupla = Let("y", Access(0, Etuple(tup2)), For("x", tup2 , Plus(Var "x", Var "y")));;

(* Funzione che calcola il valore assoluto di un numero *)
let abs = Fun("num", ITE( And( GTE(Var "num", Eint 0), Ebool(true) ), Var "num", Minus(Var "num")));;
toList (evaluate (For("x", tuponlynum, Appl(abs, Var "x"))));;
(* Cambia il segno di tutti gli interi della tupla 'tuponlynum' *)
toList (evaluate (For("x", tuponlynum, Minus(Var "x"))));;

(* Esegue lo xor tra il primo booleano della tupla e gli altri valori *)
let xor = For("z", tup2, Let("y", Access(1, Etuple(tup2)), Appl(Fun("x", And(Or(Var "x", Var "y"), Not(And(Var "x", Var "y")))), Var "z")));

(* Esempio di scoping statico *)
(* let y = 10 in let funz = fun x -> x*y in let y = 0 in funz 2   => 10 *)
evaluate (Let("y", Eint 10, Let("funz", Fun("x", Mul(Var "x", Var "y")), Let("y", Eint 0, Appl(Var "funz", Eint 2)))));;


