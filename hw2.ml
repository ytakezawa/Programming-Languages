(* Yoshika Takezawa
Due Date: 2/23/19
I pledge my honor that I have abided by the Stevens Honor System 

The following pertains to decision tree where 
0 is on th eleft and 1 is on the right*)

(* User defined datatypes *)
type dtree  = Leaf of int | Node of char*dtree*dtree

let tLeft = Node('w', Node('x', Leaf(2), Leaf(5)), 
                      Leaf(8))
let tRight = Node('w', Node('x', Leaf(2), Leaf(5)), 
                      Node('y', Leaf(7), Leaf(5)))

(* given dtree, returns height*)
let rec dTree_height: dtree -> int = fun t ->
  match t with 
  |Leaf (_)-> 0
  |Node(_, lt, rt) -> 1+ (max (dTree_height lt) (dTree_height rt))

(* given dtree, returns total numbers of nodes*)
let rec dTree_size: dtree -> int = fun t ->
  match t with
  |Leaf(_) -> 1
  |Node(_, lt, rt) -> 1 + (dTree_size lt) + (dTree_size rt)

(* returns list of paths to all leaves *)
let rec paths_helper: dtree -> int list -> int list list = fun t arr->
  match t with
  |Leaf(_) -> [arr]
  |Node(_, lt, rt) -> paths_helper lt (arr @ [0]) @ paths_helper rt (arr @ [1])
let dTree_paths: dtree -> int list list = fun t ->
  paths_helper t []

(* returns true if the tree is perfect *)
let rec dTree_is_perfect: dtree -> bool = fun t->
  match t with
  |Leaf(_) -> true
  |Node(i, lt, rt) -> dTree_height lt = dTree_height rt

(* applies a char-function to internal nodes
and applies an int-function to leaves *)
let rec dTree_map : (char -> char) -> (int -> int) -> dtree -> dtree = 
  fun f g t->
    match t with 
    |Leaf(i) -> Leaf (g i)
    |Node(i, lt, rt) -> Node(f i, dTree_map f g lt, dTree_map f g rt)

  
(*----------- Boolean function to decison tree territory ------------*)
(* creates a tree given an array of characters *)
let rec list_to_tree : char list -> dtree = fun l ->
  match l with 
  | [] -> Leaf(0)
  | x::xs -> Node(x, list_to_tree xs, list_to_tree xs)

(* given aboolean function, will replace leaves with specified values via 
specified path*)
let rec rla_helper : dtree -> int list -> int -> dtree = fun t g repl_var -> 
  match t, g with 
  |Leaf(_), [] -> Leaf(repl_var)
  |Node(i, lt, rt), x::xs -> 
    if x=0 then Node(i, rla_helper lt xs repl_var, rt)
    else Node(i, lt, rla_helper rt xs repl_var)
  |_, _ -> failwith "HEY SOMETHING wENT WRONG"
let rec replace_leaf_at : dtree -> (int list * int) list -> dtree = 
  fun t f ->
  match f with 
  |[] -> t
  |x::xs -> 
    let (l,i) = x
    in let new_tree = rla_helper t l i
    in replace_leaf_at new_tree xs

(* takes a boolean function and returns a decision tree 
using the above functions *)
let bf_to_dTree : char list * (int list * int) list -> dtree = fun bf ->
  let (ch_lst, g) = bf
  in let t = list_to_tree ch_lst
  in replace_leaf_at t g