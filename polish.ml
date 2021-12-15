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


(* (18 / 6) + 8 *)
(** Lire une expression *)
(* 5 + 3 + 4 = ["+"; "5"; "+"; "3"; "4"] *)

(* type expr_tree =
  | Nil
  | Node of expr_tree * string * expr_tree


let rec load_expr_tree (l : string list) : unit =
  let expr_stack = Stack.create()
  in let f (e: string) : unit =
    match e with
    | "+" | "-" | "*" | "/" | "%" -> Stack.push e expr_stack
    | _ -> let left = Stack.pop expr_stack in let right = Stack.pop expr_stack in 
  in List.iter f l *)

let rec read_expr (l : string list) : expr =
  match l with 
  | [x] ->
    (match (int_of_string_opt x) with
    | Some n -> Num n
    | None -> if ((x <> "+") && (x <> "-") && (x <> "*") && (x <> "/") && (x <> "%")) then Var x
      else failwith "Erreur de syntaxe: expression non reconnue")
  | "+"::b::(c::q as ll) -> Op (Add, read_expr [b], read_expr ll)
  | "-"::b::(c::q as ll) -> Op (Sub, read_expr [b], read_expr ll)
  | "*"::b::(c::q as ll) -> Op (Mul, read_expr [b], read_expr ll)
  | "/"::b::(c::q as ll) -> Op (Div, read_expr [b], read_expr ll)
  | "%"::b::(c::q as ll) -> Op (Mod, read_expr [b], read_expr ll)
  | _ -> failwith "Erreur de syntaxe: expression non reconnue";;

read_expr ["+"; "+"; "3"; "4"; "5"];;


let read_cond (l : string list) : cond =
  match l with 
  |[] -> failwith "Liste vide"
  let rec read_cond_aux acc line = 
    match line with 
    |[] -> acc
    |x::xs -> match x with 
    |"=" -> read_expr (List.rev acc) Eq read_expr xs
    |"<>" -> read_expr (List.rev acc) Ne read_expr xs
    |"<" -> read_expr (List.rev acc) Lt read_expr xs
    |"<=" -> read_expr (List.rev acc) Le read_expr xs
    |">" -> read_expr (List.rev acc) Gt read_expr xs
    |">=" -> read_expr (List.rev acc) Ge read_expr xs
    |_ -> read_cond_aux (x::acc) xs
  in read_cond_aux [] l

let rec read_instr (niv : int) (lines : (position * string) list) : (instr * (position * string) list) =
  match lines with 
  |[] -> failwith "Liste vide"
  |x::xs -> 
    match x with p, s ->
      if String.length (String.trim s) = 0 then read_instr niv xs (*Ignorer les lignes vides ou ne contenant que des blancs*)
      else 
      let nb_ind = nb_indentations s
      in if nb_ind mod 2 <> 0 then failwith "Erreur de syntaxe: nombre d'indentations impair"
      else if (nb_ind/2) <> niv then failwith "Erreur de syntaxe: nombre d'indentations non respecté"  
      else 
      let mots = create_mots s
      in match mots with 
        |[] -> failwith "Erreur de syntaxe:?????"
        |y::ys -> match y with
          |"COMMENT" -> read_instr niv xs
          |"READ" -> match ys with 
            |[v] -> Read v
            |_-> failwith "Erreur de syntaxe: READ ne supporte pas plus d'un paramètre"
          |"PRINT" -> Print (read_expr ys)
          |"IF" -> If (read_cond ys, read_block xs)
          |"WHILE" -> While (read_cond ys, read_block xs)
          |_ -> match ys with 
            |[] -> failwith "Erreur de syntaxe:?????"
            |z::zs -> if z <> ":=" then failwith "Erreur de syntaxe:?????" 
              else match zs with 
              |[] -> failwith "Erreur de syntaxe:?????"
              |_ -> (*(Set (y, read_expr zs), xs)*) failwith "TODO"

let rec read_block (niv : int) (lines : (position * string) list) : (instr * (position * string) list) =
  match lines with 
  |[] -> failwith "Liste vide"
  |y::ys -> 
  |_ -> failwith ""

let read_polish (filename:string) : program = failwith "TODO"

  (* let polish = open_in filename
  in let lines = read_lines polish 
  in if lines = [] then []
  else [0, Read "v"];; *)


let print_polish (p:program) : unit = failwith "TODO"

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