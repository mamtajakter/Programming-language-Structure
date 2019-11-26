(* Compile with: ocamlc -g -o trees trees.ml *)

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

(* put fromList, sum1, prod1, avg1, map and negateAll here *)

(* Q1 *)
let rec fromList l = 
    match l with
        [] -> Empty
    |   head :: tail -> insert (fromList tail) head
;;

(* Q2 *)
let rec sum1 tree =
    match tree with
        Empty -> 0
    |   Node(i,j,k) -> i + (sum1 j + sum1 k)
;;
let rec prod1 tree =
    match tree with
        Empty -> 1
    |   Node(i,j,k) -> i * (prod1 j * prod1 k)
;;

let avg1 tree =
    let rec helper tree =
        match tree with
            Empty -> (0,0)
        |   Node(i,l,r) ->
                let (leftSum,leftLen) = helper l in
                let (rightSum,rightLen) = helper r in
                (i + leftSum + rightSum , 1 + leftLen + rightLen) in
    let (s,len) = helper tree in
        try s/len
        with Division_by_zero -> 0
;;
        
(* Q3 *)
let rec map f tree =
    match tree with
        Empty -> Empty
    |   Node(i,l,r) -> 
            Node(f i, map f l, map f r)
;;

(* Q4 *)
let negateAll tree=
    map (fun x -> x * -1) tree
;;

let rec fold f a t =
  match t with
      Empty -> a
    | Node(j,l,r) -> fold f (fold f (f a j) l) r

(* put sum2, prod2, and avg2 here *)
(* Q4 *)
let sum2 tree =
    fold (fun a j -> a + j) 0 tree
;;
let prod2 tree =
    fold (fun a j -> a * j) 1 tree
;;
let avg2 tree =
    let (s,l) = (fold (fun a j -> a + j) 0 tree, fold (fun a j -> a + 1) 0 tree) in
    s/l
;;


type 'a iterator = Nomore | More of 'a * (unit -> 'a iterator)

let rec iter t =
  let rec f t k =
    match t with
	Empty -> k ()
      | Node(j,l,r) -> More(j, fun () -> f l (fun () -> f r k))
  in f t (fun () -> Nomore)

(* Q7 *)

(* Two ways work, just playing around with hints from Hua *)
(*
let rec sum3 tree =
    let myitr = iter tree in
    let rec helper itr =
        match itr with
        Nomore -> 0
      | More(i,j) -> i + helper (j ()) in
    helper myitr 
;;
*)
let rec sum3 tree =
    let rec helper itr ans =
        match itr with
        Nomore -> ans
      | More(i,j) -> helper (j ()) (i+ans) in
    helper (iter tree) 0
;;
(* Works too but doesn't exit when finding ANY 0 *)
(*
let rec prod3 tree =
    let myitr = iter tree in
    let rec helper itr =
        match itr with
        Nomore -> 1
      | More(i,j) -> i * helper (j ()) in
    helper myitr 
;;
*)
let rec prod3 tree =
    let rec helper itr ans =
        match itr with
        Nomore -> ans
      | More(i,j) -> if i = 0 then helper Nomore 0 else helper (j ()) (i*ans) in
    helper (iter tree) 1
;;
let rec avg3 tree =
    let rec helper itr (s,len) =
        match itr with 
        Nomore -> s/len
      | More(i,j) -> helper (j ()) ((i+s),(len+1)) in
    helper (iter tree) (0,0)
;;

     

(* challenge problem: put optionToException and exceptionToOption here *)
(* Q8 *)
(*let optionToException ('a -> 'a option) = *)



(* a little testing -- commented out since the functions do not exist yet *)

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

