(** Projet Polish -- Analyse statique d'un mini-langage impératif *)

(** Note : cet embryon de projet est pour l'instant en un seul fichier
    polish.ml. Il est recommandé d'architecturer ultérieurement votre
    projet en plusieurs fichiers source de tailles raisonnables *)

(*****************************************************************************)
(** Syntaxe abstraite Polish (types imposés, ne pas changer sauf extensions) *)

open Types
open Read
open Reprint
open Eval
open Simpl
open Sign
open Utils

module NameTable = Map.Make(String)


(***********************************************************************)

let read_polish (filename:string) : program =
  let polish_program = open_in filename
  in let lines = read_lines polish_program
  in read_program lines;;

let print_polish (p:program) : unit = 
    print_program p 0;;

let eval_polish (p:program) : unit = 
  let env : int NameTable.t = NameTable.empty
  in (fun _ -> ()) (eval_block p env);;

let simpl_polish (p:program) : unit =
  let env : int NameTable.t = NameTable.empty
  in print_polish (simpl_program p env)

let vars_polish (p:program) : unit = failwith "TODO";;

let sign_polish (p:program) : unit = sign_program p;;

let usage () =
  print_string "Polish : analyse statique d'un mini-langage\n";
  print_string "usage: run [options] <file>\n telle que les options sont:\n
  -reprint : lire et réafficher le programme polish\n
  -eval : évaluer le programme polish\n
  -simpl : simplifier un programme polish en effectuant la propagation des constantes et l'élimination des blocs morts\n
  -vars : calcul statique des variables risquant d'être accédées avant d'être écrites \n
  -sign : analyse statique du signe possible des variables lors du déroulement du programme, et application à la détermination du risque de division par zéro. \n";;


let main () =
  match Sys.argv with
  | [|_;"-reprint";file|] -> print_polish (read_polish file)
  | [|_;"-eval";file|] -> eval_polish (read_polish file)
  | [|_;"-simpl";file|] -> simpl_polish (read_polish file)
  | [|_;"-vars";file|] -> simpl_polish (read_polish file)
  | [|_;"-sign";file|] -> simpl_polish (read_polish file)
  | _ -> usage ()

(* lancement de ce main *)
let () = main ();;
