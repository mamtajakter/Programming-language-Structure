type 'a mu = Roll of ('a mu -> 'a);;

let unroll (Roll x) = x;;

let fix f = (fun x a -> f (unroll x x) a) (Roll (fun x a -> f (unroll x x) a));;

(* Ackermann without using rec *)


let ackermann f = function
    | (0,n) -> n+1
    | (m,0) -> f(m-1,1)
    | (m,n) -> f(m-1,f(m,n-1));;

(* Should return 125 *)
fix ackermann (3 ,4);;
