let tupexample = Add(Int 23, Add(Bool true, Add(Tuple( Add(Int 45, Add(Int 7, Void)) ), Add(Bool false, Add(Int 12, Void ) ) ) ) );;
let tuponlynum = Add(Int (-1), Add(Int 33, Add(Int (-8), Add(Int (-92), Add(Int (-3), Add(Int 15, Void) ) ) ) ) );;

toList (evaluate(For("val", tupexample, Not(Var "val"))));;

(* Calcola il resto della divisione per 2 di un numero *)
let restoper2 = Fun("n", Diff(Var "n", Mul(Div(Var "n", Eint 2), Eint 2)));;
(* Restituisce true se n è pari, false altrimenti *)
let true_if_even = Fun("n", ITE( Equ( Appl(restoper2, Var "n"), Eint(0) ), Ebool(true), Ebool(false)));;

(* Applica true_if_even a tutti gli elementi della tupla tupexample *) 
toList (evaluate (For("n", tupexample, Appl(true_if_even, Var "n"))));;
toList (evaluate (For("n", tupexample, Appl(restoper2, Var "n"))));;

(* Somma il primo elemento di una tupla a tutti gli elementi della tupla stessa *)
let sommatupla = Let("y", Access(0, Etuple(tupexample)), For("x", tupexample , Plus(Var "x", Var "y")));;

(* Funzione che calcola il valore assoluto di un numero *)
let abs = Fun("num", ITE( And( GTE(Var "num", Eint 0), Ebool(true) ), Var "num", Minus(Var "num")));;
toList (evaluate (For("x", tuponlynum, Appl(abs, Var "x"))));;
(* Cambia il segno di tutti gli interi della tupla 'tuponlynum' *)
toList (evaluate (For("x", tuponlynum, Minus(Var "x"))));;

(* Esegue lo xor tra il primo booleano della tupla e gli altri valori *)
let xor = For("z", tupexample, Let("y", Access(1, Etuple(tupexample)), Appl(Fun("x", And(Or(Var "x", Var "y"), Not(And(Var "x", Var "y")))), Var "z")));
