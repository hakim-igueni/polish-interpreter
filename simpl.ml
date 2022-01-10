open Types
open Eval

let simpl_expr (exp : expr) : expr =
  let rec simpl_expr_aux exp = 
    match exp with 
    | Num(v) -> exp
    | Var(v) -> exp
    | Op(Div, Num 0, expr2) -> Num 0
    | Op(Mul, expr1, Num 0) -> Num 0
    | Op(Mul, Num 0, expr2) -> Num 0
    | Op(Mul, Num 1, expr) 
    | Op(Mul, expr, Num 1) 
    | Op(Add, Num 0, expr) 
    | Op(Add, expr, Num 0) -> (simpl_expr_aux expr)
    | Op(Div, expr1, Num 0) -> Op (Div, simpl_expr_aux expr1, Num 0)
    | Op(Mod, expr1, Num 0) -> Op (Mod, simpl_expr_aux expr1, Num 0)
    | Op(op, Num x, Num y) -> (match op with 
        |Add -> Num (x+y)
        |Sub -> Num (x-y)
        |Mul -> Num (x*y)
        |Div -> Num (x/y)
        |Mod -> Num (x mod y))
    | Op(op,expr1,expr2) -> Op(op, simpl_expr_aux expr1, simpl_expr_aux expr2)
  in let exp_simp = simpl_expr_aux exp 
  in let exp_simp_simp = (simpl_expr_aux exp_simp) 
  in if exp_simp_simp = exp_simp then exp_simp else simpl_expr_aux exp_simp_simp

let simpl_cond (c : cond) : cond =
  match c with (exp1, comp, exp2) -> 
    let exp1_simp = simpl_expr exp1 
    in let exp2_simp = simpl_expr exp2 
    in (exp1_simp, comp, exp2_simp) 
    
let rec simpl_instr (pos : position) (inst : instr) (env : int NameTable.t): (instr * bool) =
  match inst with 
  | Set (v,expr) -> Set (v,simpl_expr expr), false
  | Read (v) -> inst, false
  | Print (expr) -> Print (simpl_expr expr), false
  | If (cond,blockIf,blockElse) -> 
      let cond_simp = simpl_cond cond 
      in (match cond_simp with  
        | (Num (v1), comp, Num (v2)) -> 
          if eval_cond pos cond_simp env then If (cond_simp, (simpl_program blockIf env), []), true
          else If (cond_simp, [], (simpl_program blockElse env)), true
        | _ -> If (cond_simp, simpl_program blockIf env, simpl_program blockElse env), false)
  | While(cond,bloc) -> 
    let cond_simp = simpl_cond cond 
    in (match cond_simp with  
    | (Num (v1), comp, Num (v2)) -> While (cond_simp, simpl_program bloc env), not (eval_cond pos cond_simp env) 
    | _ -> While (cond_simp, simpl_program bloc env), false)
and simpl_program (p:program) (env : int NameTable.t): program =
  match p with 
  |[] -> []
  |(pos,ins)::ps -> 
    let ins_simp, can_be_deleted = simpl_instr pos ins env
    in if can_be_deleted then 
      (match ins_simp with 
       | If (_, b1, b2) -> (simpl_program b1 env) @ (simpl_program b2 env) @ (simpl_program ps env)
       | _ -> simpl_program ps env)
    else ((pos, ins_simp)::(simpl_program ps env)) 