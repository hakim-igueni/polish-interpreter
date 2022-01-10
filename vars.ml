
module Names = Map.Make(String)
let find_vars (var_name:name) (env: bool Names.t) : bool = 
  try Names.find var_name env with
  Not_found -> false

let rec vars_expr (exp : expr) (env : bool Names.t) = 
  match exp with 
  | Num (n) -> env
  | Var (v) -> if Names.find_opt v env = None then Names.update v (fun _ -> Some false) env else env
  | Op (op, expr1, expr2) -> (match op with 
    | Add | Sub | Mul| Div | Mod -> vars_expr expr2 (vars_expr expr1 env))

let vars_cond (c:cond) (env : bool Names.t) : bool Names.t =
  let (e1, comp, e2) = c in vars_expr e2 (vars_expr e1 env)

let rec intersection_env (env1 : bool Names.t) (env2 : bool Names.t) :  bool Names.t = 
    let rec intersection_env_aux e1 e2 acc =
      Names.iter (fun k v -> let ke2 = Names.find_opt k e2 
                  in if ke2 = Some x then 
                    if x = v then (Names.add k v acc)) e1
      match l1 with 
        | [] -> acc
        | hd::tl ->
          if List.mem hd l2 then intersection_aux tl l2 (hd::acc) else intersection_aux tl l2 acc
    in intersection_aux l1 l2 []

let rec vars_if (p:program) (env : bool Names.t) :  bool Names.t = 
  let env_if : bool Names.t = Names.empty 
  in match p with
  | [] -> env_if
  | (pos, Set (v, exp))::reste ->  vars_if reste ( Names.find_opt v env_if = None then Names.update v (fun _ -> Some true) (vars_expr exp env_if) 
      else (vars_expr exp env))
  | (pos, Read (name))::reste ->  vars_if reste (if Names.find_opt name env = None then Names.update name (fun _ -> Some true) env else env) (*;print_newline()*)
  | (pos, Print (exp))::reste ->  vars_if reste (vars_expr exp env)
  | (pos, If (cond, bloc1, bloc2))::reste -> env(*let c = eval_cond cond env in if c then eval_polish_aux reste (eval_polish_aux bloc1 env) else eval_polish_aux reste (eval_polish_aux bloc2 env) *)
  | (pos, While (cond, bloc))::reste -> env 



 (*let vars_polish (p:program) : unit = 
  let e : bool Names.t = Names.empty 
  in *)
 and  vars_polish_aux (p:program) (env : bool Names.t) : bool NameTable.t = (*int NameTable.t =*)
    match p with
    | [] -> env
    | (pos, Set (v, exp))::reste -> vars_polish_aux reste (if Names.find_opt v env = None then Names.update v (fun _ -> Some true) (vars_expr exp env) else (vars_expr exp env))
    | (pos, Read (name))::reste ->  vars_polish_aux reste (if Names.find_opt name env = None then Names.update name (fun _ -> Some true) env else env) (*;print_newline()*)
    | (pos, Print (exp))::reste ->  vars_polish_aux reste (vars_expr exp env)
    | (pos, If (cond, bloc1, bloc2))::reste -> vars_polish_aux reste (union_env (vars_if bloc1 (vars_cond cond env)) (vars_if bloc2 env))(*let c = eval_cond cond env in if c then eval_polish_aux reste (eval_polish_aux bloc1 env) else eval_polish_aux reste (eval_polish_aux bloc2 env) *)
    | (pos, While (cond, bloc))::reste -> vars_polish_aux reste (union_env (vars_while bloc (vars_cond cond env)));;
    (* in let nothing (ee : bool Names.t) : unit = 
      Names.iter (fun k v -> print_string (k ^ " ")) ee; print_newline ();
      Names.iter (fun k v -> if v = false then print_string (k ^ " ")) ee
    in nothing (vars_polish_aux p e);;  *)
    (*Names.iter (fun k v -> print_string k) env *** eval_polish_aux reste (eval_while cond bloc env)*)
  (*and eval_while (cond:cond) (bloc:program) (env : int NameTable.t) : int NameTable.t =
    if eval_cond cond env then (eval_while cond bloc (eval_polish_aux bloc env)) else env*)