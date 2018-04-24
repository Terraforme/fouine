open Types

let rec ccont (e : expr_f) = match e with
  | Cst  c -> Fun(Var_Pat "_k", Fun(Var_Pat "_kE", App(Var "_k",Cst c)))
  | Var  x -> Fun(Var_Pat "_k", Fun(Var_Pat "_kE", App(Var "_k",Var x)))
  | Unit   -> Fun(Var_Pat "_k", Fun(Var_Pat "_kE", App(Var "_k",Unit)))
  | Bool b -> Fun(Var_Pat "_k", Fun(Var_Pat "_kE", App(Var "_k",Bool b)))
  | Bang e -> Fun(Var_Pat "_k", Fun(Var_Pat "_kE", App(App(ccont e,
                  Fun(Var_Pat "_addr", App(Var "_k",Bang(Var "_addr")))),Var "_kE")))

  | Neg e  -> Fun(Var_Pat "_k", Fun(Var_Pat "_kE", App(App(ccont e,
                  Fun(Var_Pat "_b", App(Var "_k", Neg(Var "_b")))),Var "_kE")))

  | PrInt e -> 
      Fun(Var_Pat "_k", Fun(Var_Pat "_kE", App(App(ccont e,Fun(Var_Pat "_v", 
      App(Var "_k",PrInt(Var "_v")))),Var "_kE")))
  | Bin (e1, op, e2) -> 
      Fun(Var_Pat "_k", Fun(Var_Pat "_kE", App(App(ccont e2,Fun(Var_Pat "_v2", 
      App(App(ccont e1,Fun(Var_Pat "_v1", App(Var "_k",Bin(Var "_v1",op,Var "_v2")))),
      Var "_kE"))),Var "_kE")))


  
  | If (be, e) -> failwith "Obsolète"  
  | IfElse(be, e1, e2) -> 
      Fun(Var_Pat "_k", Fun(Var_Pat "_kE", App(App(ccont be,Fun(Var_Pat "_b", 
      App(App(IfElse(Var "_b", ccont e1, ccont e2),Var "_k"),Var "_kE"))),Var "_kE")))
  | Let(x, e1, e2)     ->
      Fun(Var_Pat "_k", Fun(Var_Pat "_kE", App(App(ccont e1,
      Fun(x, App(App(ccont e2, Var "_k"),Var "_kE"))),Var "_kE")))
  | LetRec(f, e1, e2)  -> 
      Fun(Var_Pat "_k", Fun(Var_Pat "_kE", App(App(ccont e1,Fun(Var_Pat "_val1", 
      LetRec(f, Var "_val1", App(App(ccont e2,Var "_k"),Var "_kE")))),Var "_kE")))
  | Match (e, p) -> failwith "TODO"
  
  | Fun(x, e)   ->
      Fun(Var_Pat "_k", Fun(Var_Pat "_kE", App(Var "_k",Fun(x, ccont e))))
  | App(e1, e2) -> 
      Fun(Var_Pat "_k", Fun(Var_Pat "_kE", App(App(ccont e2,Fun(Var_Pat "_v", 
      App(App(ccont e1,Fun(Var_Pat "_f", App(App(App(Var "_f",Var "_v"),Var "_k"),
      Var "_kE"))),Var "_kE"))),Var "_kE")))

  | Aff (e1, e2) -> 
      Fun(Var_Pat "_k", Fun(Var_Pat "_kE", App(App(ccont e2,Fun(Var_Pat "_v", App(App(ccont e1,
      Fun(Var_Pat "_addr", App(Var "_k",Aff(Var "_addr",Var "_v")))),Var "_kE"))),Var "_kE")))
  | Alloc e      ->
      Fun(Var_Pat "_k", Fun(Var_Pat "_kE", App(App(ccont e,Fun(Var_Pat "_v", 
      App(Var "_k",Alloc(Var "_v")))),Var "_kE")))
  
  | Pair(e1, e2) -> 
      Fun(Var_Pat "_k", Fun(Var_Pat "_kE", App(App(ccont e2,Fun(Var_Pat "_v2", 
      App(App(ccont e1,Fun(Var_Pat "_v1", App(Var "_k",Pair(Var "_v1" , Var "_v2")))),
      Var "_kE"))),Var "_kE")))
  
  | Try(e1, xE, e2) ->
      Fun(Var_Pat "_k", Fun(Var_Pat "_kE", App(App(ccont e1,Var "_k"),
      Fun(Var_Pat xE, App(App(ccont e2,Var "_k"),Var "_kE")))))
  | Raise e         ->
      Fun(Var_Pat "_k", Fun(Var_Pat "_kE", App(App(ccont e,
      Fun(Var_Pat "_vE", App(Var "_kE",Var "_vE"))),Var "_kE")))


let ctransform e = 
  App(App(ccont e,Fun(Var_Pat "_x", Var "_x")),Fun(Var_Pat "_xE", Raise( Var "_xE")))

