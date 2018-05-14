open Types

(* La mémoire est un tableau qui pourra être modifié.
On retient une variable globale available qui pointe
sur la prochaine case libre dans la mémoire *)

let mem_limiter = ref true

let mem = ref (Array.make 1000000 Unit)
let available = ref 0;;

let new_mem () = Array.make 1000000 Unit
let init_mem () = available := 0
;;

let incr_mem () = mem := Array.append (!mem) (new_mem ())

let alloc_mem value =
  if !available >= Array.length !mem then 
    begin if !mem_limiter then failwith "Out of Memory"
          else incr_mem ()
    end;
	
  incr available;
  (!mem).(!available - 1) <- value;
  !available - 1
;;

let set_mem addr value = 
	(!mem).(addr) <- value
;;

let read_mem addr =
  (!mem).(addr)
;;
