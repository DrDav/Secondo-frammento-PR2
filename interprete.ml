(* Eccezioni *)
exception EmptyEnv;; (* Ambiente vuoto - quello di default *)

(* Tipi per la sintassi astratta *)
type ide = string;;

type eval = 
	  Int of int
	| True
	| False
	(*| Funval of efun*)
	| Tuple of etuple
(*and efun = exp * env*)
and etuple = Void | Add of eval * etuple;;
