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

# Organisation du travail

On a utilisé la technique "Pair Programming" qui est une technique de développement logiciel Agile issue de la programmation Extrême (XP).

# Misc (remarques, suggestions, questions...)

TODO