# Identifiants:

| Nom      | Prénom     | Identifiant_GitLab | Numéro_Etudiant |
| :------- | ---------- | ------------------ | --------------- |
| Benameur | Abdesselam | @benameur          | 22114518        |
| Igueni   | Hakim      | @iguenih           | 22124265        |

# Fonctionnalités

- Lecture et réaffichage d'un programme Polish
- Evaluation d'un programme Polish
- Simplification d'un programme Polish en effectuant la propagation des constantes et l'élimination des blocs morts
- Calcul statique des variables risquant d'être accédées avant d'être écrites
- Analyse statique du signe possible des variables lors du déroulement du programme, et application à la détermination du risque de division par zéro.

# Compilation et exécution
## Comment compiler le projet ?
- Exéctutez la comamnde `make`
## Comment exécuter le projet ?
> Nous nous sommes pas servis de bibliothèques externes

Exéctutez la comamnde `./run [option] [filename]`  (en donnant les options acceptées par votre programme).
Les options sont:

- `-reprint` : lire et réafficher le programme polish dont le nom est `filename`
- `-eval` : évaluer le programme polish dont le nom est `filename`
- `-simpl` : simplifier un programme polish dont le nom est `filename`
- `-vars` : calcul statique des variables risquant d'être accédées avant d'être écrites
- `-sign` : analyse statique du signe possible des variables lors du déroulement du programme, et application à la détermination du risque de division par zéro.

# Découpage modulaire
Le projet se décompose en plusieurs modules où chaque module regroupe les fonctions necessaires aux developpement de chacune des fonctionnalités possibles.   
Module read qui regroupe les fonctions necessaires pour la lecture d'un programme polish 
Module reprint regroupant les fonctions qui permettent de réafficher un programme polish 
Module eval qui regroupe les fonctions utilisés par la fonction eval_polish afin d'evaluer un programme polish
Module simpl qui regroupe les fonctions de simplification d'un programme polish 
Module vars qui regroupe les fonctions nécessaires au développement de la fonctionnalité vars
Module sign qui regroupe les fonctions permettant les bon déroulement de la fonctionnalité sign
Module types qui regroupe tout les types et toutes les structures de données utilisées dans le projet 
Module utils qui regroupe toutes les autres fonctions auxiliaires utilisées
Module polish qui regroupe toutes les fonctionnalités possibles sur un programme polish
# Organisation du travail

On a utilisé la technique "Pair Programming" qui est une technique de développement logiciel Agile issue de la programmation Extrême (XP).