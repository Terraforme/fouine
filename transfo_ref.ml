open Env
open Types
open Mem

let rec transforme_ref expr = match expr with
  | Cst c -> Fun(Var_Pat "__s", Pair(Cst c , Var "__s" ))
(*fun s -> (c,s)*)
  | Bool b -> Fun(Var_Pat "__s" , Pair(Bool b, Var "__s" ))
(*fun s -> (b,s)*)
  | Var v -> Fun(Var_Pat "__s" , Pair(Var v , Var "__s" ))
(*fun s -> (v,s)*)
  | Bang expr0 -> Fun(Var_Pat "__s" , Let(Pair_Pat(Var_Pat "__l",Var_Pat "__s1"), App(transforme_ref expr0, Var "__s" ),
  Let(Var_Pat "__v", App(App(Var "read", Var "__s1"),Var "__l"), Pair(Var "__v" , Var "__s1"))))
(*fun s -> let (l,s1) = [[e]] s in
  let v = read s1 l in (v,s1)*)
  | Unit -> Fun(Var_Pat "__s" , Pair(Unit, Var "__s" ))
(*fun s -> ((),s)*)
  | Pair(expr1, expr2) -> Fun(Var_Pat "__s" , Let(Pair_Pat(Var_Pat "__v2",Var_Pat "__s2"), App(transforme_ref expr2,Var "__s" ),
  Let(Pair_Pat(Var_Pat "__v1" ,Var_Pat "__s1"), App(transforme_ref expr1,Var "__s2" ), Pair(Pair(Var "__v1"  , Var "__v2") , Var "__s1" ))))
(*fun s -> let (v2,s2) = [[e2]] s in
  let (v1,s1) = [[e1]] s2 in
  ((v1,v2),s1)*)

  | Neg(expr0) -> Fun(Var_Pat "__s", Let(Pair_Pat(Var_Pat "__b", Var_Pat "__s0"), transforme_ref expr0, Pair(Neg(Var "__b") , Var "__s0")))
(*fun s -> let (b,s0) = [[e0]] in
  (not b, s0)*)


  | Bin(expr1, op, expr2) -> Fun(Var_Pat "__s" , Let(Pair_Pat(Var_Pat "__v2" ,Var_Pat "__s2" ), App(transforme_ref expr2,Var "__s" ),
  Let(Pair_Pat(Var_Pat "__v1" ,Var_Pat "__s1" ), App(transforme_ref expr1,Var "__s2" ), Pair(Bin(Var "__v1" , op, Var "__v2" ) , Var "__s1" ))))
(*fun s -> let (v2,s2 ) = [[e2]] s in
  let (v1,s1) = [[e1]] s2 in
  (v1 op v2,s1)*)
  | PrInt expr0 -> Fun(Var_Pat "__s" , Let(Pair_Pat(Var_Pat "__v0" ,Var_Pat "__s0" ), App(transforme_ref expr0,Var "__s" ), Pair(PrInt(Var "__v0" ) , Var "__s0" )))
(*fun s -> let (v0,s0) = [[e0]] s in
  (prInt v0, s0)*)
  | Let(pat, expr1, expr2) -> Fun(Var_Pat "__s", Let(Pair_Pat(pat, Var_Pat "__s1"), App(transforme_ref expr1,Var "__s"),
  App(transforme_ref expr2,Var "__s1")))
(*fun s -> let (pat,s1) = [[e1]] s in
  [[e2]] s1*)
  | LetRec(v, expr1, expr2) -> Fun(Var_Pat "__s", LetRec(v, Let(Pair_Pat(Var_Pat "__f", Var_Pat "__s0"), App(transforme_ref expr1,Var "__s"), Var "__f"),
  App(transforme_ref expr2, Var "__s")))
(*fun s -> let rec v = (let (f,s0) = e1______ s in f)
	in e2____ s
;;*)
  | Match(expr0, patm) -> failwith "TODO"
  | IfElse(bexpr, expr1, expr2) -> Fun(Var_Pat "__s" , Let(Pair_Pat(Var_Pat "__b0", Var_Pat "__s0" ), App(transforme_ref bexpr,Var "__s" ),
  IfElse(Var "__b0", App(transforme_ref expr1,Var "__s0" ), App(transforme_ref expr2,Var "__s0" ))))
(*fun s -> let (b0, s0) = b____ s in
	if b0 then e1_____ s0
	else e2_____ s0*)
  | Fun(pat, expr0) -> Fun(Var_Pat "__s" , Pair(Fun(pat, transforme_ref expr0) , Var "__s" ))
(*fun s -> ((fun pat -> [[e]]), s)*)
  | App(expr1, expr2) ->
      Fun(Var_Pat "__s", Let(Pair_Pat(Var_Pat "__v2", Var_Pat "__s2"), App(transforme_ref expr2,Var "__s"),
      Let(Pair_Pat(Var_Pat "__f1", Var_Pat "__s1"), App(transforme_ref expr1,Var "__s2"),
      App(App(Var "__f1",Var "__v2"),Var "__s1"))))




  (*Fun(Var_Pat "__s" , Let(Pair_Pat(Var_Pat "v2" , Var_Pat "s2" ), App(transforme_ref expr2,Var "s" ),
  Let(Pair_Pat(Var_Pat "f1", Var_Pat "s1" ), App(transforme_ref expr1,Var "s2" ), Pair(App(Var "f1",Var "v2" ) , Var "s1" )))) *)
(*fun s -> let (v2,s2) = [[e2]] s in
  let (f1,s1) = [[e1]] s2 in
  let (r, s0) = f1 v2 s1 *)
  | Aff(expr1, expr2) -> Fun(Var_Pat "__s" , Let(Pair_Pat(Var_Pat "__l1",Var_Pat "__s1" ), App(transforme_ref expr1,Var "__s" ), Let(Pair_Pat(Var_Pat "__v2" ,Var_Pat "__s2" ),
  App(transforme_ref expr2,Var "__s1" ), Let(Var_Pat "__s3", App(App(App(Var "write",Var "__s2" ),Var "__l1"),Var "__v2" ), Pair(Unit , Var "__s3")))))
(*fun s -> let (l1,s1 ) = [[e1]] s in
let (v2,s2) = [[e2]] s2 in
let s3 = write s2 l1 v2 in
( (), s3)*)
  | Alloc(expr0) -> Fun(Var_Pat "__s", Let(Pair_Pat(Var_Pat "__v", Var_Pat "__s1"), App(transforme_ref expr0,Var "__s"), Let(Pair_Pat(Var_Pat "__s2", Var_Pat "__l"),
  App(Var "alloc",Var "__s1"), Let(Var_Pat "__s3", App(App(App(Var "write",Var "__s2"),Var "__l"),Var "__v"), Pair(Var "__l" , Var "__s3")))))
(*fun s -> let (v,s1) = e0_____ s in
  let (s2,l) = alloc s1 in
  let s3 = write s2 l v in
  (l,s3)
;;*)
  | Try(expr1, v, expr2) -> Fun(Var_Pat "__s" , Try(Let(Pair_Pat(Var_Pat "__v1" , Var_Pat "__s1" ), App(transforme_ref expr1,Var "__s" ),
  Pair(Var "__v1"  , Var "__s1" )), v, App(transforme_ref expr2,Var "__s" )))
(* FIXME: on ne peut pas prendre s1 au lieu de s car on n'est pas sur de pouvoir le définir,
mais du coup on ne se souviendra plus des changements fait avant de rencontrer le raise! *)
  | Raise expr0 -> Fun(Var_Pat "__s" , Let(Pair_Pat(Var_Pat "__v" ,Var_Pat "__s0" ), App(transforme_ref expr0,Var "__s" ), Pair(Raise( Var "__v" ) , Var "__s0" )))
(*fun s -> let (v,s0) = e0____ s in
	(raise (E v), s0)*)
;;
