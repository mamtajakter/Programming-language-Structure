(* CPS Homework: A6 *)
let problem a =
    if a > 1 then print_newline();
    print_string "===========";
    print_newline();
    print_string " Problem ";
    print_int a;
    print_newline();
    print_string "===========";
    print_newline();
;;

(* Problem 1: Low-level (trivial) functions in CPS *)
problem 1;;

let rec addk (a:int) (b:int) k =
    k (a + b)
;;

let rec subk (a:int) (b:int) k =
    k (a - b)
;;

let timesk (a:int) (b:int) k =
    k (a * b)
;;

let plusk (a:float) (b:float) k =
    k (a +. b)
;;

let take_awayk (a:float) (b:float) k =
    k (a -. b)
;;

let multk (a:float) (b:float) k =
    k (a *. b)
;;

let catk (a:string) (b:string) k =
    k (a ^ b)
;;

let consk a lst k =
    k (a::lst)
;;
(*
let consk a lst k =
    k (lst @ [a])
;;
*)

let lessk a b k =
    k (a < b)
;;

let eqk a b k =
    k (a == b)
;;

let report x =
    print_string "Result: ";
    print_int x;
    print_newline();;

addk 3 4 report;;

(* Problem 2: Nesting continuations *)
(* you have a typo in the instructions here *)
problem 2;;

let add3k a b c k =
    addk a b (fun ab -> addk ab c k)
;;

add3k 1 2 3 report;;

(* a * (b + c) *)
let mult3k a b c k =
    multk a b (fun ab -> plusk ab c k)
;;

mult3k 1. 2. 3. (fun y -> report (int_of_float y));;

(* (a + b) * c *)
let multf3k a b c k =
    plusk a b (fun ab -> multk ab c k)
;;

multf3k 1. 2. 3. (fun y -> report (int_of_float y));;

let abcdk a b c d k =
    multk b c (fun bc -> 
        plusk a bc (fun abc -> 
            multk d a (fun da ->
                plusk abc da k
            )
        )
    )
;;
abcdk 2.0 3.0 4.0 5.0 (fun y -> report (int_of_float y));;

(* Problem 3: Recursion :*)
problem 3;;

let rec factorial n = 
    if n = 0
    then 1
    else n * factorial (n-1)
;;

let rec factoriale n =
    let b = n = 0 in
        if b then 1
        else let s = n - 1 in
            let m = factoriale s in 
                n * m
;;


let rec fact_range n m =
    if n < m then 1
    else if n = 0 then 1
    else n * fact_range (n-1) m 
;;

let rec fact_rangek n m k =
    if n < m
    then k 1
    else fact_rangek (n-1) m (fun z -> k (n * z))
;;

fact_rangek 7 5 report;;

(* Problem 4: List recursion *)
problem 4;;

let rec app_all flst x =
    match flst with
      [] -> x
    | h::t -> app_all t (h x)
;;

(* returns 36 because does x + 1 then x*x *)
let rec app_all flst x =
    match flst with
      [] -> []
    | h::t -> h x :: app_all t x
;;
    (*| h::t -> let z = h x in app_all t z*)

app_all [((+) 1); (fun x -> x*x); (fun x -> 13)] 5;;

let rec app_allk flstk x k =
    match flstk with
      [] -> []
    | h::t -> h x k :: app_allk t x k
;;

app_allk [(addk 1); (fun x -> timesk x x); (fun x -> (fun k -> k 13))] 5 (fun x -> x);;


(* Problem 5: EC *)
problem 5;;

(* Direct style *)
let rec sum_wholes l =
    let helper x = sum_wholes [] in
    match l with
      [] -> 0
    | h::t -> if h < 0 then helper h
    else h + sum_wholes t
;;

let l = [1;2;3;4;5];;
let ans = sum_wholes l;;
print_int ans;;
print_newline();;

let l = [2;2;-3;4;5];;
let ans = sum_wholes l;;
print_int ans;;
print_newline();;


(* CPS *)
(*
let rec sum_wholes l k xk =
    match l with
      [] -> 0
    | h::t -> if h < 0 then xk h
              else h + sum_wholesk k xk
;;
sum_wholesk [0;-1;2;3]
*)
