open Types

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
        | [] -> failwith ("Ligne " ^ string_of_int pos ^ ": Erreur de syntaxe: ligne vide ") (* ligne vide *)
        | y::ys -> match y with
          (* | "COMMENT" -> read_instr (pos+1) niv xs *)
          | "READ" -> (match ys with 
            |[v] -> if int_of_string_opt v = None && operateur v = None then (pos+1, Read (v), xs)
              else failwith ("Ligne " ^ string_of_int pos ^ ": Erreur de syntaxe: le paramètre de READ doit être un nom de variable")
            |_-> failwith ("Ligne " ^ string_of_int pos ^ ": Erreur de syntaxe: READ ne supporte pas plus d'un paramètre"))
          | "PRINT" -> let exp, reste = read_expr pos ys in 
                      if reste = [] then (pos+1, Print (exp), xs)
                      else failwith ("Ligne " ^ string_of_int pos ^ ": Erreur de syntaxe: la syntaxe de PRINT n'est pas respectée !")
          | "IF" ->
            let condition = read_cond pos ys
            in let (new_pos1, bloc_if, reste1) = read_block (pos+1) (niv+1) xs
            in let (new_pos2, bloc_else, reste3) = read_else (if bloc_if = [] then new_pos1 else (new_pos1+1)) niv reste1
            in (new_pos2, If (condition, bloc_if, bloc_else), reste3)  
          | "WHILE" -> 
            let condition = read_cond pos ys (* on lit la condition du while *)
            in let (new_pos, bloc, reste) = read_block (pos+1) (niv+1) xs (* on lit le bloc du while *)
            in (new_pos, While (condition, bloc), reste)
          | _ -> (match ys with 
            | [] -> failwith ("Ligne " ^ string_of_int pos ^ ": Erreur de syntaxe: instruction inconnue !!")
            | ":="::zs ->
              let exp, reste = read_expr pos zs
              in (if reste = [] then (pos+1, Set(y, exp), xs)
              else failwith ("Ligne " ^ string_of_int pos ^ ": Erreur de syntaxe: On peut pas affecter plus d'une expression"))
            | _ -> failwith ("Ligne " ^ string_of_int pos ^ ": Erreur de syntaxe: instruction non reconnue !"))
and read_else (pos : position) (niv : int) (lines : (position * string) list) : (position * block * (position * string) list) =
  match lines with
    | [] -> (pos, [], []) (*(new_pos1, If (condition, bloc_if, []), [])*)
    | (pp,ss)::reste2 ->
      let motss = create_mots ss in (match motss with 
        | [] -> read_else (pos+1) niv reste2
        | "ELSE" :: tl ->
          (if tl = [] then
            let nb_ind = nb_indentations ss
            in if nb_ind mod 2 <> 0 then failwith ("Ligne " ^ string_of_int pos ^ ": Erreur de syntaxe: nombre d'indentations impair !")
            else if (nb_ind/2) <> niv then failwith ("Ligne " ^ string_of_int pos ^ ": Erreur de syntaxe: nombre d'indentations non respecté !")
            else let (new_pos2, bloc_else, reste3) = read_block (pos+1) (niv+1) reste2 (* lire le bloc de ELSE *)
            in (new_pos2, bloc_else, reste3)
          else failwith ("Ligne " ^ string_of_int pos ^ ": Erreur de syntaxe: le mot clé ELSE doit être tout seul dans la ligne"))
        | _ -> (pos, [], lines))
and read_block (pos: position) (niv : int) (lines : (position * string) list) : (position * block * (position * string) list) =
  match lines with 
  | [] -> (pos, [], [])
  | (p, line)::resteLignes ->
    let nb_ind = nb_indentations line
    in if nb_ind mod 2 <> 0 then failwith ("Ligne " ^ string_of_int pos ^ ": Erreur de syntaxe: nombre d'indentations impair !")
    else if (nb_ind/2) > niv then failwith ("Ligne " ^ string_of_int pos ^ ": Erreur de syntaxe: nombre d'indentations non respecté !")
    else if (nb_ind/2) < niv then (pos, [], lines)
    else match String.split_on_char ' ' (String.trim line) with
      | "COMMENT"::_ | [""] -> read_block (pos+1) niv resteLignes (* on ignore les commentaires et les listes vides *)
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