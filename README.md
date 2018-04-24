# fouine


## Important : À propos du rendu 

  Depuis le rendu intermédiaire : la fonction `eval` a subi une grosse refonte. Maintenant, `eval` est implémenté par continuations. Les transformations de continuations et impératives ont été traitées, et les options `-outcode`, `-E -R -ER -RE` ont été ajoutées.
  L'interpréteur est capable d'afficher joliment les valeurs, ainsi que les endroits où il plante pour aider au déboggage.
  
  Concernant les tests, il s'agit de l'exécutable `testing.sh` qui s'exécute tel quel. Il teste aussi les transformations par continuations et impératives. Il affiche un `E` jaune si la transformation par continuation ne donne pas le même résultat que CamL, un `R` jaune si la transformation impérative ne donne pas le même résultat que CamL, et un `P` jaune en cas de crash dans le parsing.
  Si le test "normal" (ie sans transformation) ne donne pas le même résultat que CamL, le script met la ligne en rouge avec une croix X, et rajoute des informations rapides de déboggage plus haut (section `Debug`). Si cela ne suffit pas, on peut toujours faire à la main
  
  `./main.native [Tests/test] -debug`, `-debug` étant une option fourre-tout de déboggage qui essaie de donner le plus d'informations possible (arbre d'expression parsé, expression pretty-printée, les deux transformations classiques, les évaluations de chaque transformation (ainsi que la version non transformée), et les valeurs renvoyées de chaque version affichées joliment).
  
 *Attention* : pour le pretty-printer de valeurs, en cas de valeur fonctionnelle récursive, on ne termine pas (car le printer affiche aussi la clôture dans laquelle la fonction est encore définie)


## README version Documentation

	On a mis à jour la documentation latex, dans le répertoire /latex. Il contient les informations nécessaires pour comprendre l'interpréteur et le parseur. Il y a des commentaires disséminés un peu partout dans le code qui aideront à la compréhension.
	Certains codes restent néanmoins "obscurs" en commentaires, notamment le printer, et autres. Faire un pretty_printer qui essaie de minimiser le nombre de parenthèses et de faire une belle indentation serait presque un projet en soi, donc veuillez-nous excuser s'il est parfois peu performant. Il est possible qu'il donne parfois des codes ambigus, car il ne met pas toujours de parenthèses pour éviter de surcharger.


	On s’intéresse à fouine , un sous-ensemble de Caml qui est décrit en <http://www.ens-lyon.fr/DI/?p=5451> . Le but est d’écrire un interprète, mais attention, pas au sens d’un programme interactif qui propose, comme OCaml, de saisir des expressions au clavier et de les  évaluer dans la foulée. L’interprète prend en entrée un fichier Caml, exécute le code qui s’y trouve, et affiche ce qu’on lui demande d’afficher.


--
# Répartition Du Travail

	L'architecture globale (i.e ce qui est types) a été discuté et débattu comme il se doit en binôme.
	Ensuite, Victor Boone s'est concentré sur l'interpréteur.
		     Gabrielle Pauvert s'est concentrée sur le parseur.
	Les fichiers de tests et la doc ont été fait un peu à deux. Le LateX est de Victor, car il aime bien taper de la doc.
	
	Gabrielle s'est occupée des transformations impératives, et Victor des transformations par continuations.

--

# DOCUMENTATION

	Cf le latex.

	La liste des types est donnée dans `types.ml`. Par convention, tous les types reliés à l'interpréteur portent l'extension `_f` pour être faciles à distinguer.

	Le type des expressions fouines est `expr_f`.

	Une expression fouine renvoie un type `val_f` à l'exécution. Ainsi, les variables contiennent des valeurs de types `val_f`, qui comprend donc les entiers et les fonctions. 

# Expressions Arithmétiques

	Elles comprennent les opérations +, -, *, /, mod. Toutes leurs opérandes doivent être entières. Si ce n'est pas le cas une exception est levée. Les tests arithmétiques sont faits dans les Tests/arith.
	Les prioritées opératoires sont : `+ - < * < / mod`
	Les expressions de types `1 -------- 1` sont supportées.

# Expressions Booléennes

	Il y a deux types d'opérations : 
* Les comparaisons, nécessairement entre entiers (sinon : exception) `<, <=, >, >=, =, <>`
* Les opérateurs booléens : `||, &&`
	
	Il n'est pas autorisé de faire `if a then ...` car `a` n'est pas considéré comme étant une expression booléenne. Les expressions booléennes ne sont trouvables que dans les conditions d'un `if`.



## IMPORTANT

	Une documentation un peu plus importante sur "notre fouine" est fournie dans un pdf écrit en LateX que vous trouverez dans le dossier `latex/`.
	Celle-ci reste assez générale, mais permet de comprendre la construction globale du code. Si vous voulez encore plus de documentation, il y en a un peu dans le code. Et lisez tout simplement le code si vous en voulez encore plus !

**Les tests**
	Nos tests sont dans `Tests/`. Un script automatique peut être lancé avec `./testing.sh`
	On n'a pas renommé l'interpréteur en `fouine` mais, si vous êtes allergique à `main.native`, vous pouvez tout-à-fait faire un petit `mv main.native fouine`, ça marchera aussi.


**BUGS**
	cf `Tests/fun-rec0`  et la documentation LateX


## CONCLUSION

	On vous souhaite une chouette correction.
