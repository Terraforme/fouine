open Types

(* Fonctionnement de la mémoire :

C'est un dictionnaire int -> val_f
On donne un entier addr, et la mémoire
renvoie la valeur associée à cet entier *)

module Mem_f = Map.Make(Int32)

let init () =
  Mem_f.empty 

let set addr val mem =
  Mem_f.add (*Int32.of_int*) addr val mem

let read addr mem =
  Mem_f.find (*Int32.of_int*) addr mem
