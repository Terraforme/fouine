# fouine



	On s’intéresse à fouine , un sous-ensemble de Caml qui est décrit en http://www.ens-lyon.fr/DI/?p=5451 . Le but est d’écrire un interprète, mais attention, pas au sens d’un programme interactif qui propose, comme OCaml, de saisir des expressions au clavier et de les  évaluer dans la foulée. L’interprète prend en entrée un fichier Caml, exécute le code qui s’y trouve, et affiche ce qu’on lui demande d’afficher.


--

# DOCUMENTATION

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


# Environnement `env_f`

	C'est une liste `(var_f * val_f) list` d'affectation.
	Quand on affecte une variable à une valeur, on rajoute cette association en tête de liste (`O(1)` pour toutes les associations),, et quand on cherche la valeur d'une variable, on prend la première trouvée dans la liste (`O(n)` sur la taille de la liste).

# Fonctions  - Fonctions récursives

	Le type d'une valeur fonctionnelle est `var_f * expr_f * env_f`. Ainsi, les valeurs fonctionnelles sont déjà 'décompactées' sous la formule `fun x -> expr` munies de leur clôture. Lorsqu'on définit une fonction via un `let f = fun x -> ...`, la clôture est définie comme l'environnement courant, qui n'est qu'un pointeur. La structure de l'environnement assure le bon fonctionnement des fonctions par la suite. 
	Il est important de noter que lors d'un `let f = fun ...` on ne peut pas utiliser la fonction de manière récursive car la clôture passée en argument ne contient pas la définition de `f`. Quand on fait un `let rec`, la clôture passée en argument est la clôture dans laquelle on définit `f`, donc la clôture est définie de manière récursive dans l'interpréteur.


# A faire :

__Débutant__

* expressions arithmétiques
* `let ... in`
* `if then else`
* la fonction `prInt`

__Intermédiaire__

* fonctions et clôtures
* fonctions récursives

__Avancé__

* aspects impératifs : `:=` `!` `;` `ref` `()`
* couples
* listes
* types sommes
