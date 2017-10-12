
type inttree = Empty | Node of int * inttree * inttree

(* use this function in fromList *)
let rec insert t i =
  match t with
      Empty -> Node(i,Empty,Empty)
    | Node(j,l,r) -> 
      if i=j 
      then t 
      else if i < j 
      then Node(j,insert l i,r)
      else Node(j,l,insert r i)
		
(* no need for this function; it is just an example *)
let rec member t i =
  match t with
      Empty -> false
    | Node(j,l,r) -> i=j || (i < j && member l i) || member r i

(* put fromList, sum1, prod1, avg1, map, and negateAll here *)

let rec fromList alist= 
	match alist with 
	[]-> 	Empty 
	| x::rest-> insert (fromList rest) x

let rec sum1 t = 
	match t with 
	 Empty->0
	| Node(j,l,r)-> j+ sum1 t 

let rec prod1 t = 
	match t with 
	Empty->1
	| Node(j,l,r)-> j* sum1 t

let rec helper t = 
	match t with 
	 Empty->0 
  |Node(j,l,r) -> (helper t) + (helper l) +  (helper r) +1

let avg1 t=
	match t with 
	 Empty->0
	| Node(j,l,r)->sum1 t / helper t

let rec map f t= 
	match t with 
	 Empty->Empty 
	| Node(j,l,r) -> Node(f j,l,r)

let rec negateAll t=  map (fun j-> -j) t

let rec fold f a t =
  match t with
      Empty -> a
    | Node(j,l,r) -> fold f (fold f (f a j) l) r

(* put sum2, prod2, and avg2 here *)

 let rec sum2 t = 
	match t with 
| Empty->0
| Node(j,l,r)-> fold (fun i j->j+i) j t+ sum2 t

let rec prod2 t = 
	match t with 
| Empty->1
| Node(j,l,r)-> fold (fun i j->j*i) j t* prod2 t

let avg2 t=
	match t with 
	| Empty->0
	| Node(j,l,r)-> sum2 t / helper t;;

type 'a iterator = 
	Nomore 
	| More of 'a * (unit -> 'a iterator)

let rec iter t =
  let rec f t k =
    match t with
	     Empty -> k ()
      | Node(j,l,r) -> More(j, fun () -> f l (fun () -> f r k))
  in f t (fun () -> Nomore)(*TAKES TREE and returns iterator*)

let rec helpsum it s= 
	match it with 
	| Nomore->s 
	| More (j, f)->helpsum (f()) (s+j) (*how to send a tree*)
 
(*	let rec helpsum it = match it with Nomore->Nomore | More (j, f)-> f () (*how to send a tree*)
 	*)
let sum3 t =
	helpsum (iter t) 0
	
	let rec helpprod it s= 
	match it with 
	| Nomore->s 
	| More (j, f)->helpprod (f()) (s*j) (*how to send a tree*)
 
let prod3 t =
	helpprod (iter t) 1


let rec helpavg it s= 
	match it with 
	| Nomore->s 
	| More (j, f)->helpavg (f()) (s*j) (*how to send a tree*)
 
let avg3 t =sum3 t/helper t
	
	
(*1. helper function should take any other argument besides  the iterator ?

2. What would be the task of the helper function and what it should return?
rec helper takes iterator and unit function and return the partial same thing like sum

3. i need to call iter function from  helpersum3 function and sum function will call helper

*)

(*dan grossman*)
(* challenge problem: put optionToException and exceptionToOption here *)

(* a little testing -- commented out since the functions do not exist yet *)

(*
let tr = fromList [0;1;2;3;4;5;6;7;8;9;9;9;1] (* repeats get removed *)
let print_ans f t = print_string (string_of_int (f t)); print_string "\n"
let _ = print_ans sum1 tr
let _ = print_ans prod1 tr
let _ = print_ans avg1 tr
let _ = print_ans sum2 tr
let _ = print_ans prod2 tr
let _ = print_ans avg2 tr
let _ = print_ans sum3 tr
let _ = print_ans prod3 tr
let _ = print_ans avg3 tr
*)
