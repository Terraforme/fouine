open Types

(* La mémoire est un tableau qui pourra être modifié.
On retient une variable globale available qui pointe
sur la prochaine case libre dans la mémoire *)

let mem = Array.make 1000000 Unit
let available = ref 0;;

let init_mem () = available := 0
;;

let alloc_mem value =
  if !available = 1000000 then failwith "Out of Memory"
	else begin
         incr available;
         mem.(!available - 1) <- value;
         !available - 1
       end
;;

let set_mem addr value = 
	mem.(addr) <- value
;;

let read_mem addr =
  mem.(addr)
;;
