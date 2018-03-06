# fouine



	On s’intéresse à fouine , un sous-ensemble de Caml qui est décrit en http://www.ens-lyon.fr/DI/?p=5451 . Le but est d’écrire un interprète, mais attention, pas au sens d’un programme interactif qui propose, comme OCaml, de saisir des expressions au clavier et de les  évaluer dans la foulée. L’interprète prend en entrée un fichier Caml, exécute le code qui s’y trouve, et affiche ce qu’on lui demande d’afficher.


--

# DOCUMENTATION

	La liste des types est donnée dans `types.ml`. Par convention, tous les types reliés à l'interpréteur portent l'extension `_f` pour être faciles à distinguer.

	Le type des expressions fouines est `expr_f`.
	Le type des expressions booléennes est `bexpr_f`.

	Une expression fouine renvoie un type `val_f` à l'exécution. Ainsi, les variables contiennent des valeurs de types `val_f`, qui comprend donc les entiers et les fonctions.

# Expressions Arithmétiques

	Elles comprennent les opérations +, -, *, /, mod. Toutes leurs opérandes doivent être entières. Si ce n'est pas le cas une exception est levée.
	Les prioritées opératoires sont : `+ - < * < / mod`
	Les expressions de types `1 -------- 1` sont supportées.

# Expressions Booléennes

	Il y a deux types d'opérations : 
* Les comparaisons, nécessairement entre entiers (sinon : exception) `<, <=, >, >=, =, <>`
* Les opérateurs booléens : `||, &&`


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
