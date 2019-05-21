(* Yoshika Takezawa
3/3/19
I pledge my honor that I have abided by the stevens honor system *)
open Ast
open Ds

(* helper functions *)
let is_empty_list = function
  |[] -> true
  | _ -> false

let is_empty_tree = function
  | Empty -> true
  | _ -> false

let is_treeVal = function
  | TreeVal t -> true
  | _ -> false

let rec tail_helper = function 
  | x::[] -> [x]
  | x::xs -> tail_helper xs
  | _ -> failwith("Something went wrong")

(* main eval interpreter *)
let rec eval (en:env) (e:expr):exp_val =
  match e with
  | Int n           -> NumVal n
  | Var x           -> lookup en x
  | Let(x, e1, e2)  ->
    let v1 = eval en e1  in
    eval (extend_env en x v1) e2
  | IsZero(e1)      ->
    let v1 = eval en e1  in
    let n1 = numVal_to_num v1 in
    BoolVal (n1 = 0)
  | ITE(e1, e2, e3) ->
    let v1 = eval en e1  in
    let b1 = boolVal_to_bool v1 in
    if b1 then eval en e2 else eval en e3
  | Sub(e1, e2)     ->
    let v1 = eval en e1 in
    let v2 = eval en e2  in
    NumVal ((numVal_to_num v1) - (numVal_to_num v2))
  | Add(e1, e2)     -> 
    let v1 = eval en e1 in
    let v2 = eval en e2 in
    NumVal((numVal_to_num v1) + (numVal_to_num v2))
  | Div(e1, e2)     -> 
    let v1 = eval en e1 in
    let v2 = eval en e2 in
    NumVal((numVal_to_num v1) / (numVal_to_num v2))
  | Mul(e1, e2)     -> 
    let v1 = eval en e1 in
    let v2 = eval en e2 in
    NumVal((numVal_to_num v1) * (numVal_to_num v2))
  | Abs(e1)         -> 
    let v = numVal_to_num (eval en e1)
    in if v < 0 then NumVal(0-v)
    else NumVal(v)
  | Cons(e1, e2)    -> 
    if (is_listVal (eval en e2)) 
    then ListVal ((eval en e1)::(listVal_to_list (eval en e2)))
    else failwith("The second argument is not a list!")
  | Hd(e1)          -> 
    let a = listVal_to_list(eval en e1)
    in (match a with 
          |[] -> failwith ("Argument is an emptylist.")
          |x::_ -> x)
  
  (* for tail funtion: singleton returns emptylists*)
  | Tl(e1)          -> 
    let a = listVal_to_list(eval en e1) 
    in (let len = List.length a 
      in (match len with
          | 0 -> failwith("Argument is an emptylist")
          | 1 -> ListVal [] 
          | _ -> ListVal (tail_helper a)))
  | Empty(e1)       -> (* supports both lists and trees *)
    (match (eval en e1) with
      |ListVal _->
        (let a = listVal_to_list(eval en e1)
        in (match a with
          |[] -> BoolVal true
          |x::_ -> BoolVal false))
      |TreeVal _->
        (let b = treeVal_to_tree (eval en e1)
        in if b=Empty then BoolVal true
        else BoolVal false)
      |_ -> failwith ("Argument is not a list or tree"))
  | EmptyList       -> ListVal []
  | EmptyTree       -> TreeVal Empty
  | Node(e1,lt,rt)  -> 
    if (is_treeVal(eval en lt) && is_treeVal(eval en rt)) then 
      (let a = treeVal_to_tree (eval en lt) 
      in let b = treeVal_to_tree (eval en rt)
      in let c = eval en e1
      in TreeVal( Node(c, a, b)))
    else failwith ("Argument(s) are not trees!")
  
  (* For CaseT: given tree e1, evaluates e2 if e1 is empty
  else evaluates e3 while extending the enviornment *)
  | CaseT(e1,e2,id_e,id_lt,id_rt,e3) -> 
    if (is_treeVal(eval en e1)) then 
      (match eval en e1 with 
        |TreeVal Empty -> eval en e2
        |TreeVal Node(a, b, c) -> 
          let ev1 = extend_env en id_e (a)
          in let ev2 = extend_env ev1 id_lt (TreeVal b)
          in let ev3 = extend_env ev2 id_rt (TreeVal c)
          in eval ev3 e3
        |_ -> failwith("Something went awry"))
    else failwith("Target is not a tree")


(***********************************************************************)
(* Everything above this is essentially the same as we saw in lecture. *)
(***********************************************************************)

(* Parse a string into an ast *)
let parse s =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast


(* Interpret an expression *)
let interp (e:string):exp_val =
  e |> parse |> eval (empty_env ())
