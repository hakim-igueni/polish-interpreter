open Types
module NameTable = Map.Make(String);;

let find (pos : position) (var_name:name) (env: 'a NameTable.t) : 'a = 
  try NameTable.find var_name env
  with Not_found -> failwith ("Ligne " ^ string_of_int pos ^ ": Erreur de syntaxe: La variable " ^ var_name ^ " n'existe pas dans l'environnement")

let rec union l1 l2 =
  match l2 with
  | [] -> l1
  | hd::tl ->
    if List.mem hd l1 then union l1 tl else union (hd::l1) tl;;

let distribute l1 l2 f g cas_base =
  let rec distribute_aux s1 l =
    match l with 
    | [] -> cas_base
    | s2::q -> g (f s1 s2) (distribute_aux s1 q)
  in let rec distribute_aux2 l1 l2 =
    match l1 with 
    | [] -> cas_base
    | s1::q1 -> g (distribute_aux s1 l2) (distribute_aux2 q1 l2)
  in distribute_aux2 l1 l2;;

let intersection l1 l2 =
  let rec intersection_aux l1 l2 acc =
    match l1 with 
      | [] -> acc
      | hd::tl ->
        if List.mem hd l2 then intersection_aux tl l2 (hd::acc) else intersection_aux tl l2 acc
  in intersection_aux l1 l2 []
  