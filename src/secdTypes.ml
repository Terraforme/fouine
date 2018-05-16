open Types



type mval_f =
    INT of int
  | BOOL of bool
  | UNIT
  | ENV  of menv_f
  | CLO  of int * menv_f
  | ADDR of int
  | PTR  of int
  | PAIR of mval_f * mval_f
and  menv_f = (var_f * mval_f) list

type stack_f = mval_f list
type xstack_f = int * menv_f * stack_f list
;;
