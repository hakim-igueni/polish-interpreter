val simpl_expr : Types.expr -> Types.expr
val simpl_cond : Types.cond -> Types.cond
val simpl_instr :
  int -> Types.instr -> int Eval.NameTable.t -> Types.instr * bool
val simpl_program : Types.block -> int Eval.NameTable.t -> Types.block
