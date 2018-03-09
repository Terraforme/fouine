let f x = if x <= 0 then 1 else f (x - 1) ;;
prInt (f 1)
