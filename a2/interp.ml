(* This file is the only one you should modify for problems 2 and 3 *)
open Ast
  
exception InterpUnimplemented
exception DoNotTakeStepWithSkip

(* There are simpler heap definitions, but these will be easier
   for extending/modifying to support pushheap, popheap, pushvar, and popvar *)
let empty_heap = []

let rec get_var heap str = 
  match heap with
      [] -> 0
    | (str',i,l)::tl -> if str=str' then i else get_var tl str 

let rec  set_var heap str num = 
  match heap with
      [] -> [(str,num,[])]
    | (str',i,l)::tl -> if str=str' 
                      then (str,num,l)::tl
                      else (str',i,l)::(set_var tl str num)

let rec interp_exp h e =
  let interp_exp_r = interp_exp h in
  match e with
      Int i -> i
    | Var v -> get_var h v
    | Plus(e1,e2)  -> (interp_exp_r e1) + (interp_exp_r e2)
    | Times(e1,e2) -> (interp_exp_r e1) * (interp_exp_r e2)

(* Used for testing *)
let poph l h e =
  let tmp = get_var h e in
  match l with
    [] -> ([], h, Skip)
  | head::tail -> (tail, set_var head e tmp, Skip)

let rec push_var h s =
  match h with
    [] -> [(s,0,[])]
  | (e,n,l)::tail -> if e = s
		     then (e,n,n::l)::tail
		     else (e,n,l)::(push_var tail s)
(* Helper functions for testing *)
(* ---------------------------- *)
(* Removes first element of list *)
let removeFirst l =
  match l with
    [] -> []
  | h::t -> t

(* Returns first element of list *)
let firstElem l =
  match l with 
    [] -> 0
  | h::t -> h
(* ---------------------------- *)

(* Could be more elegant, but this is super clear*)
let rec pop_var h s =
  match h with
    [] ->[(s,0,[])]
  | (e,n,l)::tail -> if e = s
		  (*then (e,firstElem l,removeFirst l)::tail*)
		  then match l with
		    [] -> (e,0,[])::tail
		  | theFirst::theEnd -> (e,theFirst,theEnd)::tail
		  else (e,n,l)::(pop_var tail s)

let rec interp_step l h s =
  match s with
      Skip ->  raise DoNotTakeStepWithSkip 
    | Assign(v,e) -> (l,set_var h v (interp_exp h e), Skip)
    | Seq(Skip,s2) -> (l,h,s2)
    | Seq(s1,s2) -> 
      let (l2,h3,s3) = interp_step l h s1 in
      (l2,h3, Seq(s3,s2))
    | If(e,s1,s2) ->
      if((interp_exp h e) <= 0)
      then (l,h,s2)
      else (l,h,s1)
    | While(e,s1) -> (l,h, If(e,Seq(s1,s),Skip))
    | Pushheap -> (h::l,h,Skip) 
(*
    | Popheap(e) -> poph l h e
	(* why does this not give error w/ pushvar/popvar (which aren't defined yet) *)
*)
    | Popheap(e) -> (let tmp = get_var h e in
	  match l with
            [] -> ([],h,Skip)
          | head::tail -> (tail, set_var head e tmp, Skip))
    | Pushvar(e) -> (l, push_var h e, Skip)
    | Popvar(e) -> (l, pop_var h e, Skip)
    | _ -> raise InterpUnimplemented

let rec iter l h s =
  match (h,s) with
      (_,Skip) -> get_var h "ans"
    | _ -> let (l',h',s') = interp_step l h s in iter l' h' s'
  
let interp = iter [] empty_heap

(* the next two bindings really belong in a "main" module, but this
keeps all the code you need in one place *)

(* expect one command-line argument, a file to parse and interpret *)
(* you do not need to understand this interaction with the system *)
let get_prog () =
  let argv = Sys.argv in
  let _ = 
    if Array.length argv != 2
    then (prerr_string ("usage: " ^ argv.(0) ^ " [file-to-interpret]\n");
	  exit 1) in
  let ch = open_in argv.(1) in
  Parse.program Lex.lexer (Lexing.from_channel ch)

let _ =
  let prog = get_prog () in
  print_string ("ans = " ^ string_of_int (interp prog) ^"\n")
