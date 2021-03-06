
(* This file defines expressed values and environments *)


(* expressed values and environments are defined mutually recursively *)

type exp_val =
  | NumVal of int
  | BoolVal of bool
  | ProcVal of string*Ast.expr*env
  | UnitVal
  | RefVal of int
  | TaggedVariantVal of string*exp_val list
and
  env =
  | EmptyEnv
  | ExtendEnv of string*exp_val*env
  | ExtendEnvRec of string*string*Ast.expr*env

(* Operations on environments *)

let empty_env (x:unit):env = EmptyEnv

let extend_env (env:env) (id:string) (v:exp_val):env = ExtendEnv(id,v,env)

let extend_env_list (env:env) (ids:string list) (vs:exp_val list):env
  = List.fold_left (fun en (id,ev) -> ExtendEnv (id,ev,en)) env
    (List.combine (List.rev ids) (List.rev vs))

let rec apply_env (env:env) (id:string):exp_val option =
  match env with
  | EmptyEnv -> None
  | ExtendEnv (key,value,env) ->
    if id=key
    then Some value
    else apply_env env id
  | ExtendEnvRec(key,param,body,en) ->
    if id=key
    then Some (ProcVal(param,body,env))
    else apply_env en id


(* operations on expressed values *)

let numVal_to_num =  function
  |  NumVal n -> n
  | _ -> failwith "Expected a number!"

let boolVal_to_bool =  function
  |  BoolVal b -> b
  | _ -> failwith "Expected a boolean!"

let refVal_to_int =  function
  |  RefVal n -> n
  | _ -> failwith "Expected a reference!"

let rec string_of_expval = function
  |  NumVal n -> "NumVal " ^ string_of_int n
  | BoolVal b -> "BoolVal " ^ string_of_bool b
  | ProcVal (id,body,env) -> "ProcVal ("^id^","^Ast.string_of_expr body^","^ string_of_env env^")"
  | UnitVal -> "UnitVal " 
  | RefVal i -> "RefVal ("^string_of_int i^")"
  | TaggedVariantVal(tag,args) -> "Variant "^tag^"("^(List.fold_left
                                  (fun s ev -> s^","^string_of_expval
                                                  ev) "" args) ^")"
and
  string_of_env  = function
  | EmptyEnv -> ""
  | ExtendEnv(id,v,env) -> "("^id^","^string_of_expval v^")"^string_of_env env
  | ExtendEnvRec(id,param,body,env) -> "("^id^","^param^","^Ast.string_of_expr body^")"^string_of_env env
