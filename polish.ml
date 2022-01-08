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

let environment : int NameTable.t = NameTable.empty;; (* L'environnement de notre programme Polish *)

(** Lire le fichier en entrée et extraire toutes ses lignes en couplant chaque ligne à son numéro de ligne *)
let read_lines (file:in_channel) : (position * string) list = 
  let rec read_lines_aux (file:in_channel) (acc:(position * string) list) (pos: position): (position * string) list =
    try
      let x = input_line file
      in read_lines_aux file ((pos, x)::acc) (pos+1)
    with End_of_file -> acc
  in List.rev (read_lines_aux file [] 1)

let nb_indentations (line : string) : int =
  if String.split_on_char ' ' (String.trim line) = [""] then 0
  else let mots = String.split_on_char ' ' line
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
let rec read_expr (pos : position) (l : string list) : (expr * string list) = match l with 
  | [] -> failwith ("Ligne " ^ string_of_int pos ^ ": Erreur de syntaxe: expression non valide")
  | hd :: tl ->
    (match operateur hd with
      | None ->
        (match (int_of_string_opt hd) with
          | Some n -> Num n, tl
          | None -> Var hd, tl)
      | Some op -> 
        let exp1, reste1 = read_expr pos tl
        in let exp2, reste2 = read_expr pos reste1
        in (Op (op, exp1, exp2), reste2));;

let condition (s : string) : comp option =
  if s = "=" then Some Eq
  else if s = "<>" then Some Ne
  else if s = "<" then Some Lt
  else if s = "<=" then Some Le
  else if s = ">" then Some Gt
  else if s = ">=" then Some Ge
  else None;;

(** Lire une condition *)
let read_cond (pos : position) (l : string list) : cond =
  let exp1, reste1 = read_expr pos l
  in match reste1 with
  | [] -> failwith ("Ligne " ^ string_of_int pos ^ ": Erreur de syntaxe: condition non valide")
  | hd :: tl ->
    (match condition hd with
      | None -> failwith ("Ligne " ^ string_of_int pos ^ ": Erreur de syntaxe: condition non valide")
      | Some comp ->
        let exp2, reste2 = read_expr pos tl
        in if reste2 = [] then (exp1, comp, exp2)
        else failwith ("Ligne " ^ string_of_int pos ^ ": Erreur de syntaxe: condition non reconnue"))


let rec read_instr (pos: position) (niv : int) (lines : (position * string) list) : (position * instr * (position * string) list) =
  match lines with 
  |[] -> failwith "Programme vide"
  |x::xs -> 
    match x with p, s -> 
      let mots = create_mots s in match mots with 
        | [] -> failwith ("Ligne " ^ string_of_int pos ^ ": Erreur de syntaxe:?????") (* ligne vide *)
        | y::ys -> match y with
          (* | "COMMENT" -> read_instr (pos+1) niv xs *)
          | "READ" -> (match ys with 
            |[v] -> if int_of_string_opt v = None && operateur v = None then (pos+1, Read (v), xs)
              else failwith ("Ligne " ^ string_of_int pos ^ ": Erreur de syntaxe: le paramètre de READ doit être un nom de variable")
            |_-> failwith ("Ligne " ^ string_of_int pos ^ ": Erreur de syntaxe: READ ne supporte pas plus d'un paramètre"))
          | "PRINT" -> let exp, reste = read_expr pos ys in 
                      if reste = [] then (pos+1, Print (exp), xs)
                      else failwith ("Ligne " ^ string_of_int pos ^ ": Erreur de syntaxe: la syntaxe de PRINT n'est pas respectée")
          | "IF" ->
            let condition = read_cond pos ys
            in let (new_pos1, bloc1, reste1) = read_block (pos+1) (niv+1) xs
            in (match reste1 with
              | [] -> (new_pos1, If (condition, bloc1, []), [])
              | (pp,ss)::reste2 -> let motss = create_mots ss in match motss with 
                | [] -> read_instr (pos+1) niv reste2
                | "ELSE" :: tl ->
                  (if tl = [] then
                      let (new_pos2, bloc2, reste3) = read_block (new_pos1+1) (niv+1) reste2 (* lire le bloc de ELSE *)
                      in (new_pos2, If (condition, bloc1, bloc2), reste3)
                  else failwith ("Ligne " ^ string_of_int pos ^ ": Erreur de syntaxe: le mot clé ELSE doit être tout seul dans la ligne"))
                | _ -> (new_pos1, If (condition, bloc1, []), reste2))
          | "WHILE" -> (* TODO: traiter le cas ou le bloc de while ou if ou else est vide *)
            let condition = read_cond pos ys (* on lit la condition du while *)
            in let (new_pos, bloc, reste) = read_block (pos+1) (niv+1) xs (* on lit le bloc du while *)
            in (new_pos, While (condition, bloc), reste)
          | _ -> (match ys with 
            | [] -> failwith ("Ligne " ^ string_of_int pos ^ ": Erreur de syntaxe:?????")
            | ":="::zs ->
              let exp, reste = read_expr pos zs
              in (if reste = [] then (pos+1, Set(y, exp), xs)
              else failwith ("Ligne " ^ string_of_int pos ^ ": Erreur de syntaxe: On peut pas affecter plus d'une expression"))
            | _ -> failwith ("Ligne " ^ string_of_int pos ^ ": Erreur de syntaxe: instruction non reconnue !"))

and read_block (pos: position) (niv : int) (lines : (position * string) list) : (position * block * (position * string) list) =
  match lines with 
  | [] -> (pos, [], [])
  | (p, line)::resteLignes ->
    let nb_ind = nb_indentations line
    in if nb_ind mod 2 <> 0 then failwith ("Ligne " ^ string_of_int pos ^ ": Erreur de syntaxe: nombre d'indentations impair !")
    else if (nb_ind/2) > niv then failwith ("Ligne " ^ string_of_int pos ^ ": Erreur de syntaxe: nombre d'indentations non respecté")
    else if (nb_ind/2) < niv then (pos, [], lines)
    else match String.split_on_char ' ' (String.trim line) with
      | "COMMENT"::_ | [""] -> read_block (pos+1) niv resteLignes
      | _ ->
        let (new_pos1, instruction, reste1) = read_instr pos niv lines (* on lit la première instruction du bloc *)
        in let (new_pos2, bloc, reste2) = read_block new_pos1 niv reste1 (* on lit le reste du bloc *)
        in (new_pos2, ((pos, instruction)::bloc), reste2);;

let read_program (lines : (position * string) list) : program =
  match lines with 
  | [] -> []
  | _ ->
    let (new_pos, bloc, reste) = read_block 1 0 lines
    in bloc;;

let read_polish (filename:string) : program =
  let polish_program = open_in filename
  in let lines = read_lines polish_program
  in read_program lines;;

let print_ind (ind:int) : unit =
  print_string(String.make ind ' ');;

let print_op (opr : op) : unit = 
  match opr with
  |Add -> print_string " + "
  |Sub -> print_string " - "
  |Mul -> print_string " * "
  |Div -> print_string " / "
  |Mod -> print_string " % ";;

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
    (* print_newline(); *)
    print_program p 0;;
  
let p = read_polish "prog.p";;
print_polish p;;

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

(* let eval_block block = 
  ;;  *)
(* let eval_instr (instruction : instr) = 
  match instruction with 
  | Set (v,expr) -> let env = NameTable.add v (eval_expr expr) env
  | Read (n) -> begin Printf.printf "%s = " n; let v = read_int() in let env = NameTable.add n v env; end
  | Print (expr) -> Printf.printf "%s" (eval_expr expr)
  | If (cond,block1,block2) -> if eval_cond cond then eval_block block1 else eval_block2
  | While (cond,block) -> eval_while cond block
  |_ -> failwith "" *)

read_polish "prog.p";;

let find (var_name:name) (env:int NameTable.t) : int = 
  try NameTable.find var_name env with
  Not_found -> failwith ("La variable " ^ var_name ^ " n'existe pas dans l'envirronnement")

let rec eval_expr (exp: expr) (envir : position NameTable.t) : int = 
  match exp with 
  | Num (n) -> n
  | Var (v) -> find v envir
  | Op (op, expr1, expr2) -> (match op with 
    | Add -> (eval_expr expr1 envir) + (eval_expr expr2 envir)
    | Sub -> (eval_expr expr1 envir) - (eval_expr expr2 envir)
    | Mul -> (eval_expr expr1 envir) * (eval_expr expr2 envir)
    | Div ->
      let expr2_eval = eval_expr expr2 envir in if expr2_eval <> 0 then (eval_expr expr1 envir) / expr2_eval
      else failwith "Erreur d'évaluation: Division par zéro"
    | Mod ->
      let expr2_eval = eval_expr expr2 envir in if expr2_eval <> 0 then (eval_expr expr1 envir) mod expr2_eval
      else failwith "Erreur d'évaluation: Modulo par zéro");;

let eval_cond (condition:cond) (envir : 'a NameTable.t) : bool =
  match condition with
  |(expr1, comp, expr2) -> match comp with
    | Eq -> (eval_expr expr1 envir) = (eval_expr expr2 envir)
    | Ne -> (eval_expr expr1 envir) <> (eval_expr expr2 envir)
    | Lt -> (eval_expr expr1 envir) < (eval_expr expr2 envir)
    | Le -> (eval_expr expr1 envir) <= (eval_expr expr2 envir)
    | Gt -> (eval_expr expr1 envir) > (eval_expr expr2 envir)
    | Ge -> (eval_expr expr1 envir) >= (eval_expr expr2 envir)

let eval_polish (p:program) : unit = 
  let env : int NameTable.t = NameTable.empty
  in let rec eval_polish_aux (p:program) (env : 'a NameTable.t) : unit = (*int NameTable.t =*)
    match p with
    | [] -> ()
    | (pos, Set (v, exp))::reste -> eval_polish_aux p (NameTable.update v (fun a -> Some (eval_expr exp env)) env)
    | (pos, Read (name))::reste -> let n = read_int () in eval_polish_aux p (NameTable.update name (fun a -> Some n) env)
    | (pos, Print (exp))::reste -> print_int (eval_expr exp env); eval_polish_aux reste env 
    | (pos, If (cond, bloc1, bloc2))::reste -> failwith "TODO"
    | (pos, While (cond, bloc))::reste -> failwith "TODO"
  in eval_polish_aux p env;;
  
let simpl_polish (p:program) : unit = failwith "TODO"

let vars_polish (p:program) : unit = failwith "TODO"

let sign_polish (p:program) : unit = failwith "TODO"

let usage () =
  print_string "Polish : analyse statique d'un mini-langage\n";
  print_string "usage: run [options] <file>\n telle que les options sont:\n
  -reprint : lire et réafficher le programme polish\n
  -eval : évaluer le programme polish\n
  -simpl : simplifier un programme polish en effectuant la propagation des constantes et l'élimination des blocs morts\n
  -vars : TODO: \n
  -sign : TODO: \n";;


let main () =
  match Sys.argv with
  | [|_;"-reprint";file|] -> print_polish (read_polish file)
  | [|_;"-eval";file|] -> eval_polish (read_polish file)
  | [|_;"-simpl";file|] -> simpl_polish (read_polish file)
  | [|_;"-vars";file|] -> simpl_polish (read_polish file)
  | [|_;"-sign";file|] -> simpl_polish (read_polish file)
  | _ -> usage ()

(* lancement de ce main *)
<<<<<<< HEAD
let () = main ()
int_of_string "9o";;
=======
let () = main ();;
>>>>>>> 76104805d37f771fb2d95522764f8cee5feb9f42
