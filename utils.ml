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
  
  let union_env (env1 : (sign list) NameTable.t) (env2 : (sign list) NameTable.t) : (sign list) NameTable.t =
    NameTable.union (fun k l1 l2 -> Some (union l1 l2)) env1 env2;;

let inter f e1 e2 =
  let rec inter2 k v e =
    match e with
    | [] -> []
    | (k', v')::q ->
      if k = k' then [(k, f v v')] else inter2 k v q
  in let rec inter3 e1 e2 acc =
      match e1 with
      | [] -> acc
      | (k, v)::q -> inter3 q e2 (List.rev_append (inter2 k v e2) acc)
  in inter3 e1 e2 [];;

let intersection_env (env1 : (sign list) NameTable.t) (env2 : (sign list) NameTable.t) : (sign list) NameTable.t =
  let rec create_env list_of_bindings =
    match list_of_bindings with
    | [] -> NameTable.empty
    | (k, v)::q -> NameTable.add k v (create_env q)
  in let env1_bindings = NameTable.bindings env1
  in let env2_bindings = NameTable.bindings env2
  in create_env (inter intersection env1_bindings env2_bindings);;