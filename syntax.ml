(* Abstract syntax from Andrej Bauer. Licence BSD *)

(* Variable names *)
type name = string




type ty =
  | TInt              (* Integers *)
  | TBool             (* Booleans *)
  | TFloat
  | TString
  | Tchar
  | Tlist   of ty
  | Type_variant of name * (name * ty list) list (*Représente un type somme*)
  | TModule of name (*  Falloir réfléchir à une représentation simple. Pour le moment, on le met de côté*)
  | TArrow of ty * ty (* Fonctions *)



(* On ne gère que les opérateurs sur entiers, chaines et listes. TODO Float, Bool*)  
type _ expre =
  | Var                 : name    -> name expre          		(* Variable *)
  | Int                 : int     -> int expre         		(* Non-negative integer constant *)
  | Bool                : bool    -> bool expre        		(* Boolean constant *)
  | Float               : float   -> float expre
  | Char                : char    -> char expre
  | String              : string  -> string expre
  | Sequence            : 'a expre * 'b expre -> ( 'a * 'b ) expre
  | StringConcat        : 'a expre * 'b expre -> ( 'a * 'b ) expre
  | ListConcat          : 'a expre * 'a expre -> ( 'a * 'a ) expre
  | ListAddElem         : 'a expre * 'b expre -> ( 'a * 'b ) expre                             (*Beau trou de typage le 2ème argument est une liste*)
  | Times               : int expre * int expre -> ( int * int ) expre 		               (* Product [e1 * e2] *)
  | Plus                : int expre * int expre -> ( int * int ) expre   		       (* Sum [e1 + e2] *)
  | Minus               : int expre * int expre -> ( int * int ) expre  		       (* Difference [e1 - e2] *)
  | Equal               : 'a expre * 'a expre -> ( 'a * 'a ) expre	                       (* General comparison [e1 = e2] *)
  | Less                : int expre * int expre -> ( int * int ) expre    		       (* Integer comparison [e1 < e2] *)
 (* | TypeConstr          : 
  | Match               : name *) 
  | If                  : bool expre * 'a expre * 'b expre -> (bool * 'a * 'b) expre	       (* Conditional [if e1 then e2 else e3] *)
  | Fun                 : name * name * ty * ty * 'a expre -> (name * name * ty * ty * 'a) expre (* Function [fun f(x:s):t is e] 
                                                                                                  * Si on a une fonction a plusieurs paramètre, elle est réécrite comme une succession de fonctions.
                                                                                                  *)
  | Apply               : 'a expre * 'b expre -> ( 'a * 'b ) expre                                (* Application [e1 e2] *)


let get_ast s = let structur = BatOption.get s in
                match structur.Cmt_format.cmt_annots with
                | Cmt_format.Implementation st -> st
                | _                            -> failwith "pas d'annotation"

(* Exemples :
 *
 Fun ("f", "s" , TString, TString, Fun( "", "s2", TString, TString, StringConcat (Var "s", Var "s2")));;
 * *)
(*
(* Types *)

type type_expre

(* Expressions *)
type expr =
  | Var of name          		(* Variable *)
  | Int of int           		(* Non-negative integer constant *)
  | Bool of bool         		(* Boolean constant *)
  | Float of float
  | Char of char
  | String of string
  | Sequence of expr * expr
  | StringConcat of expr * expr
  | ListConcat   of expr * expr
  | Times of expr * expr 		(* Product [e1 * e2] *)
  | Plus of expr * expr  		(* Sum [e1 + e2] *)
  | Minus of expr * expr 		(* Difference [e1 - e2] *)
  | Equal of expr * expr 		(* Integer comparison [e1 = e2] *)
  | Less of expr * expr  		(* Integer comparison [e1 < e2] *)
  | If of expr * expr * expr 		(* Conditional [if e1 then e2 else e3] *)
  | Fun of name * name * ty * ty * expr (* Function [fun f(x:s):t is e] *)
  | Apply of expr * expr 		(* Application [e1 e2] *)

(* Toplevel commands *)
type toplevel_cmd =
  | Expr of expr       (* Expression *)
  | Def of name * expr (* Value definition [let x = e] *)

(* Convert a type to string *)
let string_of_type ty =
  let rec to_str n ty =
    let (m, str) =
      match ty with
	| TInt -> (2, "int")
	| TBool -> (2, "bool")
	| TArrow (ty1, ty2) -> (1, (to_str 1 ty1) ^ " -> " ^ (to_str 0 ty2))
    in
      if m > n then str else "(" ^ str ^ ")"
  in
    to_str (-1) ty

(* Convert an expression to string *)
let string_of_expr e =
  let rec to_str n e =
    let (m, str) =
      match e with
	| Int n -> (7, string_of_int n)
	| Bool b -> (7, string_of_bool b)
	| Var x -> (7, x)
	| Apply (e1, e2) -> (6, (to_str 5 e1) ^ " " ^ (to_str 6 e2))
	| Times (e1, e2) -> (5, (to_str 4 e1) ^ " * " ^ (to_str 5 e2))
	| Plus (e1, e2) -> (4, (to_str 3 e1) ^ " + " ^ (to_str 4 e2))
	| Minus (e1, e2) -> (4, (to_str 3 e1) ^ " - " ^ (to_str 4 e2))
	| Equal (e1, e2) -> (3, (to_str 3 e1) ^ " = " ^ (to_str 3 e2))
	| Less (e1, e2) -> (3, (to_str 3 e1) ^ " < " ^ (to_str 3 e2))
	| If (e1, e2, e3) -> (2, "if " ^ (to_str 2 e1) ^ " then " ^
				(to_str 2 e2) ^ " else " ^ (to_str 2 e3))
	| Fun (f, x, ty1, ty2, e) ->
	    (1, "fun " ^ f ^ "(" ^ x ^ " : " ^ (string_of_type ty1) ^ 
	       ") : " ^ (string_of_type ty2) ^ " is " ^ (to_str 0 e))
    in
      if m > n then str else "(" ^ str ^ ")"
  in
    to_str (-1) e

(* [subst [(x1,e1);...;(xn;en)] e] replaces in expression [e] all
    free occurrences of variables [x1], ..., [xn] with expressions
    [e1], ..., [en], respectively. *)
let rec subst s = function
  | (Var x) as e -> (try List.assoc x s with Not_found -> e)
  | (Int _ | Bool _) as e -> e
  | Times (e1, e2) -> Times (subst s e1, subst s e2)
  | Plus (e1, e2) -> Plus (subst s e1, subst s e2)
  | Minus (e1, e2) -> Minus (subst s e1, subst s e2)
  | Equal (e1, e2) -> Equal (subst s e1, subst s e2)
  | Less (e1, e2) -> Less (subst s e1, subst s e2)
  | If (e1, e2, e3) -> If (subst s e1, subst s e2, subst s e3)
  | Fun (f, x, ty1, ty2, e) ->
      let s' = List.remove_assoc f (List.remove_assoc x s) in
	Fun (f, x, ty1, ty2, subst s' e)
  | Apply (e1, e2) -> Apply (subst s e1, subst s e2)

*)
