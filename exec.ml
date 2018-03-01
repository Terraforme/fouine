Open Env
Open Types


(* On définit exec_f la fonction qui prend un programme, un environnement, et l'exécute
Renvoie à priori un 'int'

exec : pgm_f -> env_f -> int *)


let rec exec pgm env = match pgm with
  | Expr expr -> 1 (* TODO : evaluer l'expression *)
  | Let (x, expr, pgm_0) -> exec pgm_0 (env_aff x (1))
  (* TODO : remplacer (env_aff x (1)) par (env_aff x (calculer valeur)) *)
