open Types

let rec ccont (e : expr_f) = match e with
  | Cst  c -> Fun(Var_Pat "k", Fun(Var_Pat "kE", App(Var "k",Cst c)))
  | Var  x -> Fun(Var_Pat "k", Fun(Var_Pat "kE", App(Var "k",Var x)))
  | Unit   -> Fun(Var_Pat "k", Fun(Var_Pat "kE", App(Var "k",Unit)))
  | Bool b -> Fun(Var_Pat "k", Fun(Var_Pat "kE", App(Var "k",Bool b)))
  | Bang e -> Fun(Var_Pat "k", Fun(Var_Pat "kE", App(App(ccont e,
                  Fun(Var_Pat "addr", App(Var "k",Bang(Var "addr")))),Var "kE")))

  | Neg e  -> Fun(Var_Pat "k", Fun(Var_Pat "kE", App(App(ccont e,
                  Fun(Var_Pat "b", App(Var "k", Neg(Var "b")))),Var "kE")))

  | PrInt e -> 
      Fun(Var_Pat "k", Fun(Var_Pat "kE", App(App(ccont e,Fun(Var_Pat "v", 
      App(Var "k",PrInt(Var "v")))),Var "kE")))
  | Bin (e1, op, e2) -> 
      Fun(Var_Pat "k", Fun(Var_Pat "kE", App(App(ccont e2,Fun(Var_Pat "v2", 
      App(App(ccont e1,Fun(Var_Pat "v1", App(Var "k",Bin(Var "v1",op,Var "v2")))),
      Var "kE"))),Var "kE")))


  
  | If (be, e) -> failwith "Obsolète"  
  | IfElse(be, e1, e2) -> 
      Fun(Var_Pat "k", Fun(Var_Pat "kE", App(App(ccont be,Fun(Var_Pat "b", 
      App(App(IfElse(Var "b", ccont e1, ccont e2),Var "k"),Var "kE"))),Var "kE")))
  | Let(x, e1, e2)     ->
      Fun(Var_Pat "k", Fun(Var_Pat "kE", App(App(ccont e1,
      Fun(x, App(App(ccont e2, Var "k"),Var "kE"))),Var "kE")))
  | LetRec(f, e1, e2)  -> 
      Fun(Var_Pat "k", Fun(Var_Pat "kE", App(App(ccont e1,Fun(Var_Pat "val1", 
      LetRec(f, Var "val1", App(App(ccont e2,Var "k"),Var "kE")))),Var "kE")))
  | Match (e, p) -> failwith "TODO"
  
  | Fun(x, e)   ->
      Fun(Var_Pat "k", Fun(Var_Pat "kE", App(Var "k",Fun(x, ccont e))))
  | App(e1, e2) -> 
      Fun(Var_Pat "k", Fun(Var_Pat "kE", App(App(ccont e2,Fun(Var_Pat "v", 
      App(App(ccont e1,Fun(Var_Pat "f", App(App(App(Var "f",Var "v"),Var "k"),
      Var "kE"))),Var "kE"))),Var "kE")))

  | Aff (e1, e2) -> 
      Fun(Var_Pat "k", Fun(Var_Pat "kE", App(App(ccont e2,Fun(Var_Pat "v", App(App(ccont e1,
      Fun(Var_Pat "addr", App(Var "k",Aff(Var "addr",Var "v")))),Var "kE"))),Var "kE")))
  | Alloc e      ->
      Fun(Var_Pat "k", Fun(Var_Pat "kE", App(App(ccont e,Fun(Var_Pat "v", 
      App(Var "k",Alloc(Var "v")))),Var "kE")))
  
  | Pair(e1, e2) -> 
      Fun(Var_Pat "k", Fun(Var_Pat "kE", App(App(ccont e2,Fun(Var_Pat "v2", 
      App(App(ccont e1,Fun(Var_Pat "v1", App(Var "k",Pair(Var "v1" , Var "v2")))),
      Var "kE"))),Var "kE")))
  
  | Try(e1, xE, e2) ->
      Fun(Var_Pat "k", Fun(Var_Pat "kE", App(App(ccont e1,Var "k"),
      Fun(Var_Pat xE, App(App(ccont e2,Var "k"),Var "kE")))))
  | Raise e         ->
      Fun(Var_Pat "k", Fun(Var_Pat "kE", App(App(ccont e,
      Fun(Var_Pat "vE", App(Var "kE",Var "vE"))),Var "kE")))


let ctransform e = 
  App(App(ccont e,Fun(Var_Pat "x", Var "x")),Fun(Var_Pat "xE", Raise( Var "xE")))

