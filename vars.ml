open Types
open Utils
module NameTable = Map.Make(String)
module Vars = Set.Make(String)

 let vars_program (p:program) : unit = 
  let e : bool NameTable.t = NameTable.empty 
  in let vars = (ref Vars.empty) (* l'enseble de toutes les variables *)

  in let rec vars_expr (exp : expr) (env : bool NameTable.t) = 
  match exp with 
  | Num _ -> env
  | Var (v) -> vars := Vars.add v (!vars); if NameTable.find_opt v env = None then NameTable.update v (fun _ -> Some false) env else env
  | Op (_, expr1, expr2) -> vars_expr expr2 (vars_expr expr1 env)

  in let vars_cond (c:cond) (env : bool NameTable.t) : bool NameTable.t =
    let (e1, _, e2) = c in vars_expr e2 (vars_expr e1 env)

  in let rec vars_block (p:program) (env : bool NameTable.t) : bool NameTable.t = (*int NameTable.t =*)
  match p with
  | [] -> env
  | (pos, Set (v, exp))::reste -> vars := Vars.add v (!vars); let env_vars_expr = vars_expr exp env in vars_block reste (if NameTable.find_opt v env_vars_expr = None then NameTable.update v (fun _ -> Some true) env_vars_expr else env_vars_expr)
  | (pos, Read (name))::reste -> vars := Vars.add name (!vars); vars_block reste (if NameTable.find_opt name env = None then NameTable.update name (fun _ -> Some true) env else env) (*;print_newline()*)
  | (pos, Print (exp))::reste -> vars_block reste (vars_expr exp env)
  | (pos, If (cond, bloc1, bloc2))::reste ->
    let env_if = vars_block bloc1 (vars_cond cond env)
    in let env_else = vars_block bloc2 env 
    in let filter_fun k v = (v = false || (NameTable.find_opt k env_if <> None && NameTable.find_opt k env_else <> None))
    in vars_block reste (union_envs (&&) (NameTable.filter filter_fun (union_envs (&&) env_if env_else)) env)
  | (pos, While (cond, bloc))::reste ->
    let env_while = vars_block bloc (vars_cond cond env)
    in vars_block reste (union_envs (&&) (NameTable.filter (fun k v -> v = false) env_while) env)
    in let result = vars_block p e
    (* afficher la 1ère ligne *)
    in Vars.iter (fun s -> print_string (s ^ " ")) !vars; print_newline ();
    (* afficher la 2ème ligne *)
    (fun ee -> NameTable.iter (fun k v -> if v = false then print_string (k ^ " ")) ee; print_newline ()) (result);;