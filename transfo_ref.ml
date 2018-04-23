open Env
open Types
open Mem

let rec transforme_ref expr = match expr with
  | Cst c -> Fun(Var_Pat s, Pair(Int c , Var s))
(*fun s -> (c,s)*)
  | Var v -> Fun(Var_Pat s, Pair(Var v , Var s))
(*fun s -> (v,s)*)
  | Bang expr0 -> Fun(Var_Pat s, Let(Pair_Pat(Var_Pat l,Var_Pat s1), App(transforme_ref expr0, Var s),
  Let(Var_Pat v, App(App(Var read, Var s1),Var l), Pair(Var v , Var s1))))
(*fun s -> let (l,s1) = [[e]] s in
  let v = read s1 l in (v,s1)*)
  | Unit -> Fun(Var_Pat s, Pair(Unit, Var s))
(*fun s -> ((),s)*)
  | Pair(exrp1, expr2) -> Fun(Var_Pat s, Let(Pair_Pat(Var_Pat v2,Var_Pat s2), App(transforme_ref expr2,Var s),
  Let(Pair_Pat(Var_Pat v1,Var_Pat s1), App(transforme_ref expr1,Var s2), Pair(Pair(Var v1 , Var v2) , Var s1))))
(*fun s -> let (v2,s2) = [[e2]] s in
  let (v1,s1) = [[e1]] s2 in
  ((v1,v2),s1)*)
  | Bin(expr1, op, expr2) -> Fun(Var_Pat s, Let(Pair_Pat(Var_Pat v2,Var_Pat s2), App(Var e2,Var s), Let(Pair_Pat(Var_Pat v1,Var_Pat s1), App(Var e1,Var s2),
  Pair(Bin(Var v1, Plus, Var v2) , Var s1))))
(*fun s -> let (v2,s2) = [[e2]] s in
  let (v1,s1) = [[e1]] s2 in
  (v1 op v2,s1)*)
  | PrInt expr0 -> Fun(Var_Pat s, Let(Pair_Pat(Var_Pat v0,Var_Pat s0), App(transforme_ref expr0,Var s), Pair(prInt(Var v0) , Var s0)))
(*fun s -> let (v0,s0) = [[e0]] s in
  (prInt v0, s0)*)
  | Let(pat, expr1, expr2) -> Fun(Var_Pat s, Let(Pair_Pat(Var_Pat v1,Var_Pat s1), App(transforme_ref expr1,Var s),
  Let(Var_Pat pat, Var v1, App(transforme_ref expr2,Var s1))))
(*fun s -> let (pat,s1) = [[e1]] s in
  [[e2]] s1*)
  | LetRec(v, expr1, expr2) -> failwith "TODO"
  | Match(expr0, patm) -> failwith "TODO"
  | If(b,expr0) -> failwith "Obsolete"
  | IfElse(bexpr, expr1, expr2) ->
    (*begin
    match bexpr with
      | True ->
      | False ->
      | Cmp(expr1, c_op, expr2) ->
      | Bin_op(bexpr1, b_op, bexpr2) ->
      | Not(bexpr) ->
    end*)
    (*if a && b then c else d peut devenir transformation (if (if a then (if b then 1 else 0) else 0) = 1 then c else d)*)

  | Fun(pat, expr0) -> Fun(Var_Pat s, Pair(Fun(Var_Pat pat, transforme_ref expr0) , Var s))
(*fun s -> fun s -> ((fun pat -> [[e]]), s)*)
  | App(expr1, expr2) -> Fun(Var_Pat s, Let(Pair_Pat(Var_Pat f1, Var_Pat s1), App(transforme_ref expr1, Var s),
  Let(Pair_Pat(Var_Pat v2, Var_Pat s2), App(transforme_ref expr2,Var s1), App(App(Var f1,Var v2),Var s2))))
(**)
  | Aff(expr1, expr2) -> Fun(Var_Pat s, Let(Pair_Pat(Var_Pat l1,Var_Pat s1), App(transforme_ref expr1,Var s), Let(Pair_Pat(Var_Pat v2,Var_Pat s2),
  App(transforme_ref expr2,Var s2), Let(Var_Pat s3, App(App(App(Var write,Var s2),Var l1),Var v2), Pair(Unit , Var s3)))))
  | Alloc(expr0) -> Fun(Var_Pat s, Let(Pair_Pat(Var_Pat v,Var_Pat s1), App(transforme_ref expr0,Var s), Let(Pair_Pat(Var_Pat l,Var_Pat s2),
  App(App(Var alloc,Var s1),Var v), Pair(Var l , Var s2))))
  | Try(expr1, v, expr2) -> Fun(Var_Pat s, Try(App(transforme expr1,Var s), x, App(transforme expr2,Var s)))
  | Raise expr0 -> Fun(Var_Pat s, Let(Pair_Pat(Var_Pat v,Var_Pat s0), App(transforme expr0,Var s), Pair(Raise( Var v) , Var s0)))
(*fun s -> let (v,s0) = e0____ s in
	(raise (E v), s0)*)
;;
