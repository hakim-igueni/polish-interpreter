open Types

let print_ind (ind:int) : unit =
  print_string(String.make ind ' ');;

let print_op (opr : op) : unit = 
  match opr with
  |Add -> print_string "+ "
  |Sub -> print_string "- "
  |Mul -> print_string "* "
  |Div -> print_string "/ "
  |Mod -> print_string "% ";;

let rec print_expr (expr : expr) : unit =
  match expr with 
  | Num(v) -> print_int(v)
  | Var(v) -> print_string(v)
  | Op(op,expr1,expr2) -> print_op (op); print_expr(expr1); print_string (" "); print_expr(expr2)

let print_cond (cond : cond) : unit =
  match cond with 
  |(exp1,comp,exp2)-> 
    print_expr(exp1);
    (match comp with
    |Eq -> print_string " = "
    |Ne -> print_string " <> "
    |Lt -> print_string " < "
    |Le -> print_string " <= "
    |Gt -> print_string " > "
    |Ge -> print_string " >= ");
    print_expr(exp2)

let rec print_instr (inst : instr) (ind:int) : unit =
  match inst with
  | Set (v,expr) -> print_string(v ^ " := " );print_expr(expr);print_newline()
  | Read (v) -> print_string("READ "^v);print_newline()
  | Print(expr) -> print_string("PRINT ");print_expr(expr);print_newline()
  | If (cond,block1,block2) ->
    print_string("IF "); print_cond(cond); print_newline();
    print_program block1 (ind+2);
    if (block2 <> []) then (print_ind ind; print_string("ELSE\n"); print_program block2 (ind+2))
  | While(cond,block) -> print_string("WHILE ");print_cond(cond);print_newline();print_program block (ind+2)
and print_program (p:program) (ind : int) : unit =
  match p with 
  |[] -> ()
  |(pos,instr)::ps -> print_ind ind; print_instr instr ind;print_program ps ind;;