(** Projet Polish -- Analyse statique d'un mini-langage impératif *)

(** Note : cet embryon de projet est pour l'instant en un seul fichier
    polish.ml. Il est recommandé d'architecturer ultérieurement votre
    projet en plusieurs fichiers source de tailles raisonnables *)

(*****************************************************************************)
(** Syntaxe abstraite Polish (types imposés, ne pas changer sauf extensions) *)

(** Position : numéro de ligne dans le fichier, débutant à 1 *)
type position = int

(** Nom de variable *)
type name = string

(** Opérateurs arithmétiques : + - * / % *)
type op = Add | Sub | Mul | Div | Mod

(** Expressions arithmétiques *)
type expr =
  | Num of int
  | Var of name
  | Op of op * expr * expr

(** Opérateurs de comparaisons *)
type comp =
| Eq (* = *)
| Ne (* Not equal, <> *)
| Lt (* Less than, < *)
| Le (* Less or equal, <= *)
| Gt (* Greater than, > *)
| Ge (* Greater or equal, >= *)

(** Condition : comparaison entre deux expressions *)
type cond = expr * comp * expr

(** Instructions *)
type instr =
  | Set of name * expr
  | Read of name
  | Print of expr
  | If of cond * block * block
  | While of cond * block
and block = (position * instr) list

(** Un programme Polish est un bloc d'instructions *)
type program = block

module NameTable = Map.Make(String)

(***********************************************************************)

let env = NameTable.empty;; (* L'environnement de notre programme Polish *)

(** Lire le fichier en entrée et extraire toutes ses lignes en couplant chaque ligne à son numéro de ligne *)
let read_lines (file:in_channel) : (position * string) list = 
  let rec read_lines_aux (file:in_channel) (acc:(position * string) list) (pos: position): (position * string) list =
    try
      let x = input_line file
      in read_lines_aux file ((pos, x)::acc) (pos+1)
    with End_of_file -> acc
  in List.rev (read_lines_aux file [] 1)

let nb_indentations (line : string) : int =
  let mots = String.split_on_char ' ' line
  in let rec nb_indentations_aux mots nb =
    match mots with 
    |[] -> nb
    |m::ms -> if m = "" then nb_indentations_aux ms (nb+1) else nb
  in nb_indentations_aux mots 0

(** Créer à partir d'une chaine donnée, une liste de mots en supprimant tout les blancs qui y figurent *)
let create_mots (s : string) : string list = 
  let rec create_mots_aux l = match l with 
    |[] -> []
    |x::xs -> if x = "" then create_mots_aux xs 
      else x::(create_mots_aux xs)
  in create_mots_aux (String.split_on_char ' ' (String.trim s))

let operateur (s : string) : op option =
  if s = "+" then Some Add
  else if s = "-" then Some Sub
  else if s = "*" then Some Mul
  else if s = "/" then Some Div
  else if s = "%" then Some Mod
  else None;;

(** Lire une expression *)
let rec read_expr (l : string list) : (expr * string list) = match l with 
  | [] -> failwith "Erreur de syntaxe: expression vide"
  | hd :: tl ->
    (match operateur hd with
      | None ->
        (match (int_of_string_opt hd) with
          | Some n -> Num n, tl
          | None -> Var hd, tl)
      | Some op -> 
        let exp1, reste1 = read_expr tl
        in let exp2, reste2 = read_expr reste1
        in Op (op, exp1, exp2), reste2);;

read_expr ["+"; "+"; "3"; "4"; "5"];;

let condition (s : string) : comp option =
  if s = "=" then Some Eq
  else if s = "<>" then Some Ne
  else if s = "<" then Some Lt
  else if s = "<=" then Some Le
  else if s = ">" then Some Gt
  else if s = ">=" then Some Ge
  else None;;

(** Lire une condition *)
let read_cond (l : string list) : cond =
  let exp1, reste1 = read_expr l
  in match reste1 with
  | [] -> failwith "Erreur de syntaxe: condition non valide"
  | hd :: tl ->
    (match condition hd with
      | None -> failwith "Erreur de syntaxe: condition non valide"
      | Some comp ->
        let exp2, reste2 = read_expr tl
        in if reste2 = [] then (exp1, comp, exp2)
        else failwith "Erreur de syntaxe: condition non reconnue");;

read_expr ["1"; "="; "3"; "4"];;

(* let read_cond (l : string list) : cond =
  let rec read_cond_aux acc line = 
    match line with 
    |[] -> failwith ""
    |x::xs -> match x with 
    |"=" -> (read_expr (List.rev acc), Eq, read_expr xs)
    |"<>" -> (read_expr (List.rev acc), Ne, read_expr xs)
    |"<" -> (read_expr (List.rev acc), Lt, read_expr xs)
    |"<=" -> (read_expr (List.rev acc), Le, read_expr xs)
    |">" -> (read_expr (List.rev acc), Gt, read_expr xs)
    |">=" -> (read_expr (List.rev acc), Ge, read_expr xs)
    |_ -> read_cond_aux (x::acc) xs
  in read_cond_aux [] l;; *)

read_expr ["+";"x";"2";"<";"h"];;

let rec read_instr (pos: position) (niv : int) (lines : (position * string) list) : (position * instr * (position * string) list) =
  match lines with 
  |[] -> failwith "Programme vide"
  |x::xs -> 
    match x with p, s ->
      if String.length (String.trim s) = 0 then read_instr (pos+1) niv xs (*Ignorer les lignes vides ou ne contenant que des blancs*)
      else 
      let nb_ind = nb_indentations s
      in if nb_ind mod 2 <> 0 then failwith "Erreur de syntaxe: nombre d'indentations impair"
      else if (nb_ind/2) <> niv then failwith "Erreur de syntaxe: nombre d'indentations non respecté"  
      else let mots = create_mots s in (match mots with 
        |[] -> failwith "Erreur de syntaxe:?????" (* ligne vide *)
        |y::ys -> match y with
          |"COMMENT" -> read_instr (pos+1) niv xs (* pourquoi pas pos+1 *)
          |"READ" -> (match ys with   
            |[v] -> 
              if int_of_string_opt v = None && operateur v = None then 
                begin
                  (*let env = NameTable.add v v env;*)
                  (pos, Read (v), xs)
                end
              else failwith "Erreur de syntaxe: le paramètre de READ doit être un nom de variable"
            |_-> failwith "Erreur de syntaxe: READ ne supporte pas plus d'un paramètre")
          |"PRINT" -> let exp, reste = read_expr ys in 
                      if reste = [] then (pos, Print (exp), xs)
                      else failwith "Erreur de syntaxe: PRINT ne supporte pas plus d'une expression"
          |"IF" ->
            let condition = read_cond ys
            in let (new_pos1, bloc1, reste1) = read_block (pos+1) (niv+1) xs
            in let (new_pos2, bloc2, reste2) = read_block (new_pos1+1) (niv+1) reste1
            in (new_pos2, If (condition, bloc1, bloc2), reste2)
          |"WHILE" ->
            let condition = read_cond ys
            in let (new_pos, bloc, reste) = read_block (pos+1) (niv+1) xs
            in (new_pos, While (condition, bloc), reste)
          |_ -> (match ys with 
            |[] -> failwith "Erreur de syntaxe:?????"
            |z::zs -> if z <> ":=" then failwith "Erreur de syntaxe:?????" 
              (*else (match zs with 
              |[] -> failwith "Erreur de syntaxe:?????"
              |_ -> (Set (y, read_expr zs), xs)* failwith "TODO"*)
              else let exp, reste = read_expr zs in 
              if reste = [] then (pos, Set(y, exp), xs)
              else failwith "Erreur de syntaxe: On peut pas affecter plus d'une expression") : position * instr * (position * name) list)

and read_block (pos: position) (niv : int) (lines : (position * string) list) : (position * block * (position * string) list) =
  match lines with 
  | [] -> failwith "Bloc vide"
  | _ ->
    let (new_pos1, instruction, reste1) = read_instr pos niv lines
    in let (new_pos2, bloc, reste2) = read_block (new_pos1+1) niv reste1
    in (new_pos2, ((pos, instruction)::bloc), reste2);;

read_instr 1 0 [(1, "IF x = 7");(2, "  PRINT n")];;
let read_program (lines : (position * string) list) : program =
  match lines with 
  | [] -> failwith "Programme vide"
  | _ ->
    let (new_pos, bloc, reste) = read_block 1 0 lines
    in bloc;;

let read_polish (filename:string) : program =
  let polish = open_in filename
  in let lines = read_lines polish 
  in if lines = [] then []
  else read_program lines;;


let print_ind (ind:int) : unit =
  print_string(String.make ind ' ')

let print_op (opr : op) : unit = 
  match opr with
  |Add -> print_string " + "
  |Sub -> print_string " - "
  |Mul -> print_string " * "
  |Div -> print_string " / "
  |Mod -> print_string " % "

let rec print_expr (expr : expr) : unit =
  match expr with 
  | Num(v) -> print_int(v)
  | Var(v) -> print_string(v)
  | Op(op,expr1,expr2) -> print_expr(expr1);print_op (op);print_expr(expr2)

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
  | If (cond,block1,block2) -> print_string("IF ");print_cond(cond);print_program block1 (ind+2);print_program block2 (ind+2)
  | While(cond,block) -> print_string("WHILE ");print_cond(cond);print_newline();print_program block (ind+2)
and print_program (p:program) (ind : int) : unit =
  match p with 
  |[] -> ()
  |(pos,instr)::ps -> print_ind ind; print_instr instr ind;print_program ps ind

let print_polish (p:program) : unit = 
    print_newline();
    print_program p 0;;
  

print_polish [(2, Read "n"); (3, Set ("i", Num 1)); (4, Set ("r", Num 1));
(6,
 While ((Var "i", Le, Num 5),
  [(7, Set ("r", Op (Mul, Var "i", Var "r")));
   (8, Set ("r", Op (Mul, Var "i", Var "r")));
   (9,
    While ((Var "i", Le, Num 8),
     [(10, Set ("i", Op (Add, Var "i", Num 1)));
      (11, Set ("i", Op (Add, Var "i", Num 1)))]));
   (12, Print (Var "r"))]));
(13, Print (Var "i"))];;


let get_value v = 
  try NameTable.find v env with
  Not_found -> failwith ""

let rec eval_expr (exp: expr) =
  match exp with 
  |Num (n) -> n
  |Var (v) -> get_value v
  |Op (op, expr1, expr2) -> (match op with 
    |Add -> (eval_expr expr1) + (eval_expr expr2)
    |Sub -> (eval_expr expr1) - (eval_expr expr2 )
    |Mul -> (eval_expr expr1) * (eval_expr expr2)
    |Div -> if (eval_expr expr2) <> 0 then (eval_expr expr1) / (eval_expr expr2)
      else failwith "Division par zero"
    |Mod -> if (eval_expr expr2) <> 0 then (eval_expr expr1) mod (eval_expr expr2)
      else failwith "Modulo par zero")
  |_ -> failwith ""
let eval_cond (cond:cond) =
  match cond with
  |(expr1, comp, expr2) -> (match comp with
    | Eq -> (eval_expr expr1) = (eval_expr expr2 )
    | Ne -> (eval_expr expr1) <> (eval_expr expr2)
    | Lt -> (eval_expr expr1) < (eval_expr expr2)
    | Le -> (eval_expr expr1) <= (eval_expr expr2)
    | Gt -> (eval_expr expr1) > (eval_expr expr2)
    | Ge -> (eval_expr expr1) >= (eval_expr expr2)
    |_ -> failwith "")
  |_ -> failwith ""


let eval_block block = 
  ;;
let 
let eval_instr (instruction : instr) = 
  match instruction with 
  | Set (v,expr) -> let env = NameTable.add v (eval_expr expr) env
  | Read (n) -> begin Printf.printf "%s = " n; let v = read_int() in let env = NameTable.add n v env; end
  | Print (expr) -> Printf.printf "%s" (eval_expr expr)
  | If (cond,block1,block2) -> if eval_cond cond then eval_block block1 else eval_block2
  | While (cond,block) -> eval_while cond block
  |_ -> failwith ""


let eval_polish (p:program) : unit = failwith "TODO"

let usage () =
  print_string "Polish : analyse statique d'un mini-langage\n";
  print_string "usage: à documenter (TODO)\n" (* TODO *)

let main () =
  match Sys.argv with
  | [|_;"-reprint";file|] -> print_polish (read_polish file)
  | [|_;"-eval";file|] -> eval_polish (read_polish file)
  | _ -> usage ()

(* lancement de ce main *)
let () = main ()
int_of_string "9o";;
