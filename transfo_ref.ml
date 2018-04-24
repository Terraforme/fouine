open Env
open Types
open Mem

let rec transforme_ref expr = match expr with
  | Cst c -> Fun(Var_Pat "s", Pair(Cst c , Var "s" ))
(*fun s -> (c,s)*)
  | Bool b -> Fun(Var_Pat "s" , Pair(Bool b, Var "s" ))
(*fun s -> (b,s)*)
  | Var v -> Fun(Var_Pat "s" , Pair(Var v , Var "s" ))
(*fun s -> (v,s)*)
  | Bang expr0 -> Fun(Var_Pat "s" , Let(Pair_Pat(Var_Pat "l",Var_Pat "s1"), App(transforme_ref expr0, Var "s" ),
  Let(Var_Pat "v", App(App(Var "read", Var "s1"),Var "l"), Pair(Var "v" , Var "s1"))))
(*fun s -> let (l,s1) = [[e]] s in
  let v = read s1 l in (v,s1)*)
  | Unit -> Fun(Var_Pat "s" , Pair(Unit, Var "s" ))
(*fun s -> ((),s)*)
  | Pair(expr1, expr2) -> Fun(Var_Pat "s" , Let(Pair_Pat(Var_Pat "v2",Var_Pat "s2"), App(transforme_ref expr2,Var "s" ),
  Let(Pair_Pat(Var_Pat "v1" ,Var_Pat "s1"), App(transforme_ref expr1,Var "s2" ), Pair(Pair(Var "v1"  , Var "v2") , Var "s1" ))))
(*fun s -> let (v2,s2) = [[e2]] s in
  let (v1,s1) = [[e1]] s2 in
  ((v1,v2),s1)*)

  | Neg(expr0) -> Fun(Var_Pat "s", Let(Pair_Pat(Var_Pat "b", Var_Pat "s0"), transforme_ref expr0, Pair(Neg(Var "b") , Var "s0")))
(*fun s -> let (b,s0) = [[e0]] in
  (not b, s0)*)


  | Bin(expr1, op, expr2) -> Fun(Var_Pat "s" , Let(Pair_Pat(Var_Pat "v2" ,Var_Pat "s2" ), App(transforme_ref expr2,Var "s" ),
  Let(Pair_Pat(Var_Pat "v1" ,Var_Pat "s1" ), App(transforme_ref expr1,Var "s2" ), Pair(Bin(Var "v1" , op, Var "v2" ) , Var "s1" ))))
(*fun s -> let (v2,s2 ) = [[e2]] s in
  let (v1,s1) = [[e1]] s2 in
  (v1 op v2,s1)*)
  | PrInt expr0 -> Fun(Var_Pat "s" , Let(Pair_Pat(Var_Pat "v0" ,Var_Pat "s0" ), App(transforme_ref expr0,Var "s" ), Pair(PrInt(Var "v0" ) , Var "s0" )))
(*fun s -> let (v0,s0) = [[e0]] s in
  (prInt v0, s0)*)
  | Let(pat, expr1, expr2) -> Fun(Var_Pat "s", Let(Pair_Pat(pat, Var_Pat "s1"), App(transforme_ref expr1,Var "s"),
  App(transforme_ref expr2,Var "s1")))
(*fun s -> let (pat,s1) = [[e1]] s in
  [[e2]] s1*)
  | LetRec(v, expr1, expr2) -> Fun(Var_Pat "s", LetRec(v, Let(Pair_Pat(Var_Pat "f", Var_Pat "s0"), App(tranforme_ref expr1,Var "s"), Var "f"),
   App(transforme_ref expr2,Var "s0")))
  | Match(expr0, patm) -> failwith "TODO"
  | If(b,expr0) -> failwith "Obsolete"
  | IfElse(bexpr, expr1, expr2) -> Fun(Var_Pat "s" , Let(Pair_Pat(Var_Pat "b0", Var_Pat "s0" ), App(transforme_ref bexpr,Var "s" ),
  IfElse(Var "b0", App(transforme_ref expr1,Var "s0" ), App(transforme_ref expr2,Var "s0" ))))
(*fun s -> let (b0, s0) = b____ s in
	if b0 then e1_____ s0
	else e2_____ s0*)
  | Fun(pat, expr0) -> Fun(Var_Pat "s" , Pair(Fun(pat, transforme_ref expr0) , Var "s" ))
(*fun s -> ((fun pat -> [[e]]), s)*)
  | App(expr1, expr2) ->
      Fun(Var_Pat "s", Let(Pair_Pat(Var_Pat "v2", Var_Pat "s2"), App(transforme_ref expr2,Var "s"),
      Let(Pair_Pat(Var_Pat "f1", Var_Pat "s1"), App(transforme_ref expr1,Var "s2"),
      App(App(Var "f1",Var "v2"),Var "s1"))))




  (*Fun(Var_Pat "s" , Let(Pair_Pat(Var_Pat "v2" , Var_Pat "s2" ), App(transforme_ref expr2,Var "s" ),
  Let(Pair_Pat(Var_Pat "f1", Var_Pat "s1" ), App(transforme_ref expr1,Var "s2" ), Pair(App(Var "f1",Var "v2" ) , Var "s1" )))) *)
(*fun s -> let (v2,s2) = [[e2]] s in
  let (f1,s1) = [[e1]] s2 in
  let (r, s0) = f1 v2 s1 *)
  | Aff(expr1, expr2) -> Fun(Var_Pat "s" , Let(Pair_Pat(Var_Pat "l1",Var_Pat "s1" ), App(transforme_ref expr1,Var "s" ), Let(Pair_Pat(Var_Pat "v2" ,Var_Pat "s2" ),
  App(transforme_ref expr2,Var "s1" ), Let(Var_Pat "s3", App(App(App(Var "write",Var "s2" ),Var "l1"),Var "v2" ), Pair(Unit , Var "s3")))))
(*fun s -> let (l1,s1 ) = [[e1]] s in
let (v2,s2) = [[e2]] s2 in
let s3 = write s2 l1 v2 in
( (), s3)*)
  | Alloc(expr0) -> Fun(Var_Pat "s", Let(Pair_Pat(Var_Pat "v", Var_Pat "s1"), App(transforme_ref expr0,Var "s"), Let(Pair_Pat(Var_Pat "s2", Var_Pat "l"),
  App(Var "alloc",Var "s1"), Let(Var_Pat "s3", App(App(App(Var "write",Var "s2"),Var "l"),Var "v"), Pair(Var "l" , Var "s3")))))
(*fun s -> let (v,s1) = e0_____ s in
  let (s2,l) = alloc s1 in
  let s3 = write s2 l v in
  (l,s3)
;;*)
  | Try(expr1, v, expr2) -> Fun(Var_Pat "s" , Try(Let(Pair_Pat(Var_Pat "v1" , Var_Pat "s1" ), App(transforme_ref expr1,Var "s" ),
  Pair(Var "v1"  , Var "s1" )), v, App(transforme_ref expr2,Var "s" )))
(* FIXME: on ne peut pas prendre s1 au lieu de s car on n'est pas sur de pouvoir le définir,
mais du coup on ne se souviendra plus des changements fait avant de rencontrer le raise! *)
  | Raise expr0 -> Fun(Var_Pat "s" , Let(Pair_Pat(Var_Pat "v" ,Var_Pat "s0" ), App(transforme_ref expr0,Var "s" ), Pair(Raise( Var "v" ) , Var "s0" )))
(*fun s -> let (v,s0) = e0____ s in
	(raise (E v), s0)*)
;;
