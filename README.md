# fouine


## Important : À propos du rendu intermédiaire

	On a réparé notre rendu 1, et le parser est plus succint. On a grandement amélioré notre système de test : `testing.sh`. Les exceptions ont été implémentées mais ont rendu le code lourd voire illisible.

	Mainteant on renvoie en plus du nouvel état de la mémoire et de la valeur, une valeur d'exception, un `int option` en pratique. Si celui-ci vaut `None`, c'est qu'il n'y a pas d'exception à rattraper et qu'on peut poursuivre le calcul normalement. Ainsi, entre chaque évaluation, il faut vérifier si on n'a pas une exception sur les bras, auquel cas il faut refiler le bébé jusqu'à ce qu'un `try` le rattrape.

	On a imaginé les tests les plus vicieux possibles, mais notre "fouine" semble résister encore et toujours. On compte modifier ça et mettre des continuations, pour rendre le code plus lisible. Par ailleurs, il y a un début de pattern-matching.


## README version Documentation

	Je tiens à préciser que la documentation latex n'est pas à jour (parce qu'il faut avouer qu'on ne va pas refaire le latex à chaque fois qu'on modifie une ligne de code). Elle sera toute belle pour le rendu final. Lots of Love. Bisous.


	On s’intéresse à fouine , un sous-ensemble de Caml qui est décrit en <http://www.ens-lyon.fr/DI/?p=5451> . Le but est d’écrire un interprète, mais attention, pas au sens d’un programme interactif qui propose, comme OCaml, de saisir des expressions au clavier et de les  évaluer dans la foulée. L’interprète prend en entrée un fichier Caml, exécute le code qui s’y trouve, et affiche ce qu’on lui demande d’afficher.


--
# Répartition Du Travail

	L'architecture globale (i.e ce qui est types) a été discuté et débattu comme il se doit en binôme.
	Ensuite, Victor Boone s'est concentré sur l'interpréteur.
		     Gabrielle Pauvert s'est concentrée sur le parseur.
	Les fichiers de tests et la doc ont été fait un peu à deux. Le LateX est de Victor, car il aime bien taper de la doc.

--

# DOCUMENTATION

	Cf le latex.

	La liste des types est donnée dans `types.ml`. Par convention, tous les types reliés à l'interpréteur portent l'extension `_f` pour être faciles à distinguer.

	Le type des expressions fouines est `expr_f`.
	Le type des expressions booléennes est `bexpr_f`.

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
