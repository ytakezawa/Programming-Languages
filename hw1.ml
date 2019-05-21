(* 
Name: Yoshika Takezawa 
Due Date: Feb 8, 2019
I pledge my honor that I have abided by the Stevens Honor System. 
Program Desciption: list of instructions encoded as integers:
0 -> Pen down
1 -> Pen up
2 -> Move North
3 -> Move East
4 -> Move South
5 -> Move West
*)

(* User defined datatype*)
type program = int list

let square: program = [0; 2; 2; 3; 3; 4; 4; 5; 5; 1]
let square2: program = [0; 3; 3; 2; 2; 5; 5; 4; 4; 1]

(* Exercise 1: given a starting coord and a program, 
returns a list of colored (visited) coordinates, no duplicates*)

let get_next : int * int -> int -> int * int= fun current x -> 
  match x with
  | 2 -> let (x,y) = current in (x,y+1)
  | 3 -> let (x,y) = current in (x+1,y)
  | 4 -> let (x,y) = current in (x,y-1)
  | 5 -> let (x,y) = current in (x-1,y)
  | _ -> raise (Invalid_argument " Not a command. ");;

let rec colored_helper : int * int -> bool -> program -> (int * int) list = 
  fun current pen_down p ->
    match p with
    | [] -> []
    | 0::xs -> current :: colored_helper current true xs
    | 1::xs -> colored_helper current false xs
    | x::xs -> 
      let next = get_next current x 
      in if pen_down then next :: colored_helper next pen_down xs
        else colored_helper next pen_down xs;;

let rec find_duplicates : 'a list -> 'a list = function
| [] -> []
| x::xs -> 
  let rec fd_helper t : 'a list -> 'a list = function
  | [] -> []
  | y::ys -> 
    if t=y then fd_helper t ys
    else y::fd_helper t ys
  in x:: find_duplicates (fd_helper x xs);;

let colored : int * int -> program -> (int * int) list = fun start p ->
  let output = colored_helper start false p
  in find_duplicates output;;


(* Exercise 2: sees if two programs colors the same set of coords*)
let rec equivalent_helper: 'a list -> 'a list -> bool = fun lst1 lst2 ->
  match lst1, lst2 with
  | [],[] -> true
  | lst1,[] -> false
  | [],lst2 -> false 
  | x::xs, lst2 -> 
    let rec eh_h f = function
    | [] -> []
    | z::zs -> 
      if f=z then zs
      else z:: eh_h f zs
    in equivalent_helper xs (eh_h x lst2);;

let equivalent : program -> program -> bool = fun a b ->
  equivalent_helper (colored (0,0) a) (colored (0,0) b);;


(* Exercise 3: given a program and returns the mirrored program *)
let rec mirror_image : program -> program  = fun a ->
  match a with 
  | [] -> []
  | 0::xs -> 0:: mirror_image xs
  | 1::xs -> 1:: mirror_image xs
  | 2::xs -> 4:: mirror_image xs
  | 3::xs -> 5:: mirror_image xs
  | 4::xs -> 2:: mirror_image xs
  | 5::xs -> 3:: mirror_image xs
  | _::xs -> raise (Invalid_argument " Not a command. ");;


(* Exercise 4: given a program,
returns the program rotated 90 deg clockwise*)
let rec rotate_90 : program -> program  = fun a ->
  match a with 
  | [] -> []
  | 0::xs -> 0:: rotate_90 xs
  | 1::xs -> 1:: rotate_90 xs
  | 2::xs -> 3:: rotate_90 xs
  | 3::xs -> 4:: rotate_90 xs
  | 4::xs -> 5:: rotate_90 xs
  | 5::xs -> 2:: rotate_90 xs
  | _::xs -> raise (Invalid_argument " Not a command. ");;


(* Exercise 5: returns a list of n copies of x *)
let rec repeat : int -> 'a -> 'a list= fun n x -> 
  match n with
  | 0 -> []
  | _ -> x :: repeat (n-1) x;;


(* Exercise 6: returns program p enlarged n times *)
let rec pantograph : program -> int -> program= fun p n ->
  match p with 
  | [] -> []
  | 0::xs -> 0 :: pantograph xs n
  | 1::xs -> 1 :: pantograph xs n
  | x::xs -> repeat n x @ pantograph xs n ;;

(* Exercise 7: compresses a program by replacing adjacent of the same 
instruction with a tuple (m,n) where m is the instruction and n is the
number of times m should be executed *)
let rec get_frequency : 'a -> 'a list -> int  = fun key p -> 
  match p with
    | [] -> 1
    | x::xs -> 
      if x=key then 1 + get_frequency key xs
      else 1;;

let rec delete_front_adj : 'a -> 'a list -> 'a list = fun key p ->
  match p with
  | [] -> []
  | x::xs ->
    if x=key then delete_front_adj key xs
    else x::xs;;

let rec compress : program -> (int * int) list = fun p -> 
  match p with 
  | [] -> []
  | x::xs -> (x, get_frequency x xs) :: compress (delete_front_adj x xs);;


(* Exercise 8: decompresses a compressed program *)
let rec decompress : (int * int) list -> program = function
| [] -> []
| current::xs ->
  let (x,y) = current 
  in repeat y x @ decompress xs;;