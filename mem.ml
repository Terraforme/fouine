open Types

(* Fonctionnement de la mémoire :

C'est un dictionnaire int -> val_f
On donne un entier addr, et la mémoire
renvoie la valeur associée à cet entier *)

let available = ref Int32.zero;;

module Mem_f = Map.Make(Int32)

let init_mem () =
  Mem_f.empty

let alloc_mem value mem =
  let new_mem = Mem_f.add !available value mem in
  available := Int32.succ !available;
  (new_mem, Ref(Int32.pred !available))
;;

let set_mem addr value mem = 
  Mem_f.add (*Int32.of_int*) addr value mem
;;

let read_mem addr mem =
  Mem_f.find (*Int32.of_int*) addr mem
;;
