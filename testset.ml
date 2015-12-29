let tupexample = Add(Int 23, Add(Bool true, Add(Tuple( Add(Int 45, Add(Int 7, Void)) ), Add(Bool false, Add(Int 12, Void ) ) ) ) );;

let restoper2 = Fun("n", Diff(Var "n", Mul(Div(Var "n", Eint 2), Eint 2)));;
let true_if_even = Fun("n", ITE( Equ( Appl(restoper2, Var "n"), Eint(0) ), Ebool(true), Ebool(false)));;

evaluate (For("n", tupexample, Appl(true_if_even, Var "n")));;


