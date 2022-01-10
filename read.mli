val read_lines : in_channel -> (int * string) list
val nb_indentations : string -> int
val create_mots : string -> string list
val operateur : string -> Types.op option
val read_expr : int -> string list -> Types.expr * string list
val condition : string -> Types.comp option
val read_cond : int -> string list -> Types.cond
val read_instr :
  int ->
  int -> (int * string) list -> int * Types.instr * (int * string) list
val read_else :
  int ->
  int -> (int * string) list -> int * Types.block * (int * string) list
val read_block :
  int ->
  int -> (int * string) list -> int * Types.block * (int * string) list
val read_program : (int * string) list -> Types.block
