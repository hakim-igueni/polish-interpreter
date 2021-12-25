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

let env : int NameTable.t = NameTable.empty;; (* L'environnement de notre programme Polish *)

(** Lire le fichier en entrée et extraire toutes ses lignes en couplant chaque ligne à son numéro de ligne *)
let read_lines (file:in_channel) : (position * string) list = 
  let rec read_lines_aux (file:in_channel) (acc:(position * string) list) (pos: position): (position * string) list =
    try
      let x = input_line file
      in read_lines_aux file ((pos, x)::acc) (pos+1)
    with End_of_file -> acc
  in List.rev (read_lines_aux file [] 1)

let nb_indentations (line : string) : int =
  if line = "" then 0
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
                      else failwith ("Ligne " ^ string_of_int pos ^ ": Erreur de syntaxe: PRINT ne supporte pas plus d'une expression")
          | "IF" ->
            let condition = read_cond pos ys
            in let (new_pos1, bloc1, reste1) = read_block (pos+1) (niv+1) xs
            in (match reste1 with
              | [] -> (new_pos1, If (condition, bloc1, []), reste1)
              | (p,s)::t ->
                (if s = "ELSE" then
                  (if t = [] then
                    let (new_pos2, bloc2, reste2) = read_block (new_pos1+1) (niv+1) reste1
                    in (new_pos2, If (condition, bloc1, bloc2), reste2)
                  else failwith ("Ligne " ^ string_of_int pos ^ ": Erreur de syntaxe: le mot clé ELSE doit être tout seul dans la ligne"))
                else
                  let (new_pos2, bloc2, reste2) = read_block (new_pos1+1) (niv+1) reste1
                  in (new_pos2, If (condition, bloc1, bloc2), reste2)))
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

let print_polish (p:program) : unit = failwith "TODO";;

let op_to_string op = 
  match op with
  |Add -> print_string "+ "
  |Sub -> print_string "- "
  |Mul -> print_string "* "
  |Div -> print_string "/ "
  |Mod -> print_string "% "
let get_n n env = 
  try Hashtbl.find env n with
  Not_found -> failwith ""
let eval_expr (exp: expr) (envir : env) = 
  match exp with 
  |Num (n) -> n
  |Var (v) -> get_n v envir
  |Op (op, expr1, expr2) -> (match op with 
    |Add -> (eval_expr expr1 envir) + (eval_expr expr2 envir)
    |Sub -> (eval_expr expr1 envir) - (eval_expr expr2 envir)
    |Mul -> (eval_expr expr1 envir) * (eval_expr expr2 envir)
    |Div -> if (eval_expr expr2 envir) <> 0 then (eval_expr expr1 envir) / (eval_expr expr2 envir)
      else failwith "Division par zero"
    |Mod -> if (eval_expr expr2 envir) <> 0 then (eval_expr expr1 envir) mod (eval_expr expr2 envir)
      else failwith "Modulo par zero")
    |_ -> failwith ""
let eval_cond (cond:cond) (envir:env) =
  match cond with
  |(expr1, comp, expr2) -> match comp with
    | Eq -> (eval_expr expr1 envir) = (eval_expr expr2 envir)
    | Ne -> (eval_expr expr1 envir) <> (eval_expr expr2 envir)
    | Lt -> (eval_expr expr1 envir) < (eval_expr expr2 envir)
    | Le -> (eval_expr expr1 envir) <= (eval_expr expr2 envir)
    | Gt -> (eval_expr expr1 envir) > (eval_expr expr2 envir)
    | Ge -> (eval_expr expr1 envir) >= (eval_expr expr2 envir)

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
int_of_string "9o"