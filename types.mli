type position = int
type name = string
type op = Add | Sub | Mul | Div | Mod
type expr = Num of position | Var of name | Op of op * expr * expr
type comp = Eq | Ne | Lt | Le | Gt | Ge
type cond = expr * comp * expr
type instr =
    Set of name * expr
  | Read of name
  | Print of expr
  | If of cond * block * block
  | While of cond * block
and block = (position * instr) list
type program = block
type sign = Neg | Zero | Pos | Error
