open Types
open Utils
module NameTable = Map.Make(String)

let rec eval_expr (pos : position) (exp: expr) (envir : int NameTable.t) : int = 
  match exp with 
  | Num (n) -> n
  | Var (v) -> find pos v envir
  | Op (op, expr1, expr2) -> (match op with 
    | Add -> (eval_expr pos expr1 envir) + (eval_expr pos expr2 envir)
    | Sub -> (eval_expr pos expr1 envir) - (eval_expr pos expr2 envir)
    | Mul -> (eval_expr pos expr1 envir) * (eval_expr pos expr2 envir)
    | Div ->
      let expr2_eval = eval_expr pos expr2 envir in if expr2_eval <> 0 then (eval_expr pos expr1 envir) / expr2_eval
      else failwith ("Ligne " ^ string_of_int pos ^ ": Erreur d'évaluation: Division par zéro")
    | Mod ->
      let expr2_eval = eval_expr pos expr2 envir in if expr2_eval <> 0 then (eval_expr pos expr1 envir) mod expr2_eval
      else failwith ("Ligne " ^ string_of_int pos ^ ": Erreur d'évaluation: Modulo par zéro"));;

let eval_cond (pos : position) (condition:cond) (envir : int NameTable.t) : bool =
  match condition with
  |(expr1, comp, expr2) -> match comp with
    | Eq -> (eval_expr pos expr1 envir) = (eval_expr pos expr2 envir)
    | Ne -> (eval_expr pos expr1 envir) <> (eval_expr pos expr2 envir)
    | Lt -> (eval_expr pos expr1 envir) < (eval_expr pos expr2 envir)
    | Le -> (eval_expr pos expr1 envir) <= (eval_expr pos expr2 envir)
    | Gt -> (eval_expr pos expr1 envir) > (eval_expr pos expr2 envir)
    | Ge -> (eval_expr pos expr1 envir) >= (eval_expr pos expr2 envir);;

(* let environment : int NameTable.t = NameTable.empty;; *)

let rec eval_block (p:program) (env : int NameTable.t) : int NameTable.t = (*int NameTable.t =*)
    match p with
    | [] -> env
    | (pos, Set (v, exp))::reste -> eval_block reste (NameTable.update v (fun _ -> Some (eval_expr pos exp env)) env)
    | (pos, Read (name))::reste -> print_string (name ^ "? "); eval_block reste (NameTable.update name (fun _ -> Some (read_int ())) env) (*;print_newline()*)
    | (pos, Print (exp))::reste -> print_int (eval_expr pos exp env); print_newline (); eval_block reste env 
    | (pos, If (cond, bloc1, bloc2))::reste -> let c = eval_cond pos cond env in if c then eval_block reste (eval_block bloc1 env) else eval_block reste (eval_block bloc2 env)
    | (pos, While (cond, bloc))::reste -> eval_block reste (eval_while pos cond bloc env)
  and eval_while (pos : position) (cond:cond) (bloc:program) (env : int NameTable.t) : int NameTable.t =
    if eval_cond pos cond env then (eval_while pos cond bloc (eval_block bloc env)) else env