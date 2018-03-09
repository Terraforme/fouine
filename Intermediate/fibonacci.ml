let rec fib n =
  if n = 0 then 0
  else if n = 1 then 1
  else (fib (n - 2)) + (fib (n - 1))
let a = if fib 0 = 0 then 1 else 0
let b = if fib 1 = 1 then 1 else 0
let c = if fib 2 = 1 then 1 else 0
let d = if fib 3 = 2 then 1 else 0
let e = if fib 4 = 3 then 1 else 0
let f = if fib 5 = 5 then 1 else 0
let g = if fib 6 = 8 then 1 else 0
let h = if fib 20 = 6765 then 1 else 0;;
prInt (a * b * c * d * e * f * g * h)
