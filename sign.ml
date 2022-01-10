open Types
open Utils
module NameTable = Map.Make(String)

let sign_add (l1 : sign list) (l2 : sign list) : sign list =
  let sign_add_aux (s1 : sign) (s2 : sign) : sign list = match s1, s2 with 
    | Error, _ | _, Error -> [Error]
    | Zero, s | s, Zero -> [s]
    | Pos, Pos -> [Pos]
    | Neg, Neg -> [Neg]
    | Pos, Neg | Neg, Pos -> [Zero; Pos; Neg]
  in distribute l1 l2 sign_add_aux union []

let sign_mul (l1 : sign list) (l2 : sign list) : sign list =
  let sign_mul_aux (s1 : sign) (s2 : sign) : sign list = match s1, s2 with
  | Error, _ | _, Error -> [Error]
  | Zero, s | s, Zero -> [Zero]
  | Pos, Pos | Neg, Neg -> [Pos]
  | _, _ -> [Neg] 
  in distribute l1 l2 sign_mul_aux union []

let sign_div (l1 : sign list) (l2 : sign list) : sign list = 
  let sign_inverse s = match s with 
  | Pos | Neg -> s
  | Zero | Error -> Error
  in sign_mul l1 (List.map sign_inverse l2);;

let sign_sub (l1 : sign list) (l2 : sign list) : sign list =
  let sign_negation s = match s with 
    | Pos -> Neg
    | Neg -> Pos
    | Zero | Error -> s
  in sign_add l1 (List.map sign_negation l2);;

let sign_mod (l1 : sign list) (l2 : sign list) : sign list =
  let sign_mod_aux (s1 : sign) (s2 : sign) : sign list = match s1, s2 with
    | Error, _ | _, Error | _, Zero -> [Error]
    | Zero, s -> [Zero]
    | s, _ -> [Zero; s]
  in distribute l1 l2 sign_mod_aux union []
  
let rec sign_expr (pos : position) (exp : expr) (env : (sign list) NameTable.t) : sign list =
  match exp with 
  | Num (n) -> if n = 0 then [Zero] else if n > 0 then [Pos] else [Neg]
  | Var (v) -> find pos v env
  | Op (op, expr1, expr2) -> (match op with 
    | Add -> sign_add (sign_expr pos expr1 env) (sign_expr pos expr2 env)
    | Sub -> sign_sub (sign_expr pos expr1 env) (sign_expr pos expr2 env)
    | Mul -> sign_mul (sign_expr pos expr1 env) (sign_expr pos expr2 env)
    | Div -> sign_div (sign_expr pos expr1 env) (sign_expr pos expr2 env)
    | Mod -> sign_mod (sign_expr pos expr1 env) (sign_expr pos expr2 env));;

let condition_satisfied (pos : position) (c : cond) (env : (sign list) NameTable.t) : bool =
  let eq_satisfied l1 l2 =
    let eq_satisfied_aux s1 s2 = match s1, s2 with
      | Pos, Pos | Zero, Zero | Neg, Neg -> true
      | _, _ -> false
    in distribute l1 l2 eq_satisfied_aux (||) false
  in let gt_satisfied l1 l2 =
    let gt_satisfied_aux s1 s2 = match s1, s2 with
      | Error, _ | _, Error -> false
      | Pos, (Pos | Zero | Neg) -> true
      | (Zero | Neg), Neg -> true
      | (Zero | Neg), (Zero | Pos) -> false
    in distribute l1 l2 gt_satisfied_aux (||) false
  in let lt_satisfied l1 l2 = 
    let lt_satisfied_aux s1 s2 = match s1, s2 with
      | Error, _ | _, Error -> false
      | Neg, (Pos | Zero | Neg) -> true
      | (Zero | Pos), Pos -> true
      | (Zero | Pos), (Neg | Zero) -> false
    in distribute l1 l2 lt_satisfied_aux (||) false
  in let ne_satisfied l1 l2 = (lt_satisfied l1 l2) || (gt_satisfied l1 l2)
  in let ge_satisfied l1 l2 = (gt_satisfied l1 l2) || (eq_satisfied l1 l2)
  in let le_satisfied l1 l2 = (lt_satisfied l1 l2) || (eq_satisfied l1 l2)
  in match c with 
    | exp1, Eq, exp2 -> eq_satisfied (sign_expr pos exp1 env) (sign_expr pos exp2 env)
    | exp1, Ne, exp2 -> ne_satisfied (sign_expr pos exp1 env) (sign_expr pos exp2 env)
    | exp1, Lt, exp2 -> lt_satisfied (sign_expr pos exp1 env) (sign_expr pos exp2 env)
    | exp1, Le, exp2 -> le_satisfied (sign_expr pos exp1 env) (sign_expr pos exp2 env)
    | exp1, Gt, exp2 -> gt_satisfied (sign_expr pos exp1 env) (sign_expr pos exp2 env)
    | exp1, Ge, exp2 -> ge_satisfied (sign_expr pos exp1 env) (sign_expr pos exp2 env);;

let sign_to_string s = match s with Pos -> "+" | Neg -> "-" | Zero -> "0" | Error -> "!";;
let sign_list_to_string_list l = List.map sign_to_string l;;
let print_sign_list l = List.iter print_string (sign_list_to_string_list l);;

let sign_program (p:program) : unit = 
  let e : (sign list) NameTable.t = NameTable.empty
  in let divbyzero = false in let first_time_divbyzero = 0
    in let rec sign_block p env = 
      match p with
      | [] -> env
      | (pos, Set (v, exp))::reste -> 
          let exp_sign = sign_expr pos exp env
          in let divbyzero, first_time_divbyzero = if List.mem Error exp_sign && not divbyzero then true, pos else divbyzero, first_time_divbyzero
          in sign_block reste (NameTable.update v (fun _ -> Some exp_sign) env)
      | (pos, Read (name))::reste -> sign_block reste (NameTable.update name (fun _ -> Some [Neg; Zero; Pos]) env)
      | (pos, Print (exp))::reste ->
        let exp_sign = sign_expr pos exp env
        in let divbyzero, first_time_divbyzero = if List.mem Error exp_sign && not divbyzero then true, pos else divbyzero, first_time_divbyzero
        in env
      | (pos, If (cond, bloc1, bloc2))::reste ->
        let (exp1, _, exp2) = cond in
        let exp1_sign, exp2_sign = sign_expr pos exp1 env, sign_expr pos exp2 env
        in let divbyzero, first_time_divbyzero =
          if (List.mem Error exp1_sign || List.mem Error exp2_sign) && not divbyzero then true, pos
          else divbyzero, first_time_divbyzero
        in if condition_satisfied pos cond env then sign_block reste (sign_block bloc1 env) 
        else sign_block reste (sign_block bloc2 env)
      | (pos, While (cond, bloc))::reste ->
        let (exp1, _, exp2) = cond
        in let exp1_sign, exp2_sign = sign_expr pos exp1 env, sign_expr pos exp2 env
        in let divbyzero, first_time_divbyzero =
          if (List.mem Error exp1_sign || List.mem Error exp2_sign) && not divbyzero then true, pos
          else divbyzero, first_time_divbyzero
        in sign_block reste (sign_while pos cond bloc env)
    and sign_while (pos : position) (cond:cond) (bloc:program) (env : (sign list) NameTable.t) : (sign list) NameTable.t =
      if condition_satisfied pos cond env then (sign_while pos cond bloc (sign_block bloc env)) else env
  in NameTable.iter (fun k v -> print_string (k ^ " "); print_sign_list v; print_newline ()) (sign_block p e);
  if divbyzero then Printf.printf "divbyzero %d\n" first_time_divbyzero else print_string "safe\n";;