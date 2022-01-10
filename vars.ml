open Types
open Utils
module NameTable = Map.Make(String)

let rec vars_expr (exp : expr) (env : bool NameTable.t) = 
  match exp with 
  | Num _ -> env
  | Var (v) -> if NameTable.find_opt v env = None then NameTable.update v (fun _ -> Some false) env else env
  | Op (_, expr1, expr2) -> vars_expr expr2 (vars_expr expr1 env)

let vars_cond (c:cond) (env : bool NameTable.t) : bool NameTable.t =
  let (e1, _, e2) = c in vars_expr e2 (vars_expr e1 env)

(* let rec intersection_env (env1 : bool NameTable.t) (env2 : bool NameTable.t) :  bool NameTable.t = 
    let rec intersection_env_aux e1 e2 acc =
      NameTable.iter (fun k v -> let ke2 = NameTable.find_opt k e2 
                  in if ke2 = Some x then 
                    if x = v then (NameTable.add k v acc)) e1
      match l1 with 
        | [] -> acc
        | hd::tl ->
          if List.mem hd l2 then intersection_aux tl l2 (hd::acc) else intersection_aux tl l2 acc
    in intersection_aux l1 l2 [] *)

(* let rec vars_if (p:program) (env : bool NameTable.t) :  bool NameTable.t = 
  let env_if : bool NameTable.t = NameTable.empty 
  in match p with
  | [] -> env_if
  | (pos, Set (v, exp))::reste ->  vars_if reste ( NameTable.find_opt v env_if = None then NameTable.update v (fun _ -> Some true) (vars_expr exp env_if) 
      else (vars_expr exp env))
  | (pos, Read (name))::reste ->  vars_if reste (if NameTable.find_opt name env = None then NameTable.update name (fun _ -> Some true) env else env) (*;print_newline()*)
  | (pos, Print (exp))::reste ->  vars_if reste (vars_expr exp env)
  | (pos, If (cond, bloc1, bloc2))::reste -> env(*let c = eval_cond cond env in if c then eval_polish_aux reste (eval_polish_aux bloc1 env) else eval_polish_aux reste (eval_polish_aux bloc2 env) *)
  | (pos, While (cond, bloc))::reste -> env  *)

let union_envs env1 env2 =
  let result = NameTable.empty 
  in (fun () -> result) (NameTable.iter
    (fun k v -> NameTable.iter
                  (fun k' v' ->
                    if k = k' then (fun _ -> ()) (NameTable.add k (v || v') result)
                    else (fun _ -> ()) (NameTable.add k' v' (NameTable.add k v result))) env2) env1)

let intersect_envs env1 env2 =
  let result = NameTable.empty 
  in (fun () -> result) (NameTable.iter
    (fun k v -> NameTable.iter
                  (fun k' v' ->
                    if k = k' then (fun _ -> ()) (NameTable.add k (v && v') result) 
                    else (fun _ -> ()) (NameTable.add k' v' result)) env2) env1)

let rec vars_block (p:program) (env : bool NameTable.t) : bool NameTable.t = (*int NameTable.t =*)
  match p with
  | [] -> env
  | (pos, Set (v, exp))::reste -> let env_vars_expr = vars_expr exp env in vars_block reste (if NameTable.find_opt v env_vars_expr = None then NameTable.update v (fun _ -> Some true) env_vars_expr else env_vars_expr)
  | (pos, Read (name))::reste ->  vars_block reste (if NameTable.find_opt name env = None then NameTable.update name (fun _ -> Some true) env else env) (*;print_newline()*)
  | (pos, Print (exp))::reste ->  vars_block reste (vars_expr exp env)
  | (pos, If (cond, bloc1, bloc2))::reste ->
    let env_if = vars_block bloc1 (vars_cond cond env) in let env_else = vars_block bloc2 env 
    in vars_block reste (union_envs (intersect_envs env (intersect_envs env_if env_else)) env)
  | (pos, While (cond, bloc))::reste -> let env_while = (vars_block bloc (vars_cond cond env)) in vars_block reste (union_envs (intersect_envs env env_while) env)

 let vars_program (p:program) : unit = 
  let e : bool NameTable.t = NameTable.empty 
  in (fun ee -> 
    NameTable.iter (fun k v -> print_string (k ^ " ")) ee; print_newline ();
    NameTable.iter (fun k v -> if v = false then print_string (k ^ " ")) ee;
    print_newline ())
  (vars_block p e);;