(* On définit dans ce fichier les types principaux, nécessaires 
 * partout dans le code. *) 

(* Considérons le code très simple suivant :

let f = fun x -> 
    let y = x in
    x * y

Ici, f, x, y sont des 'identificateurs'. f désigne une fonction, 
x et y désignent des variables. 'let', 'in', 'fun' sont mots-clés.
'*' est une opération. Quant à '=' et '->', il s'agit de ponctuations.

   Il est nécessaire de donner un type pour les identificateurs,
pour les mots-clés, les opérations, variables et fonctions.

Par convention, tous nos types auront l'extension '_f' pour
spécifier qu'il s'agit de types spécifiques à 'fouine'. *)
      


type identifier_f = string

		      
type function_f = unit
type variable_f = int
type const_f    = int

type entity     = Fun of function_f | Var of variable_f | Cst of const_f
		    
type key_word_f = Let | Fun | If ;;

type expr_f =
  | Quantity of entity
  | Plus  of expr_f * expr_f
  | Minus of expr_f * expr_f
  | Times of expr_f * expr_f
  | Div   of expr_f * expr_f
  | Mod   of expr_f * expr_f
;;

(* ************ PROGRAMMES ************** *)
(* Ils sont vus comme des arbres, étiquetés par : *)
  
			
     
  
				
