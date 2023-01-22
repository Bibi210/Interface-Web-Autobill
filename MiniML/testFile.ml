
let rec fibo n = if n <= 0 then 0 else fibo (n - 1) + fibo (n - 2)

let fibo_term n =
  let rec fibo_rec v prec ante =
    if v <= 1
    then prec
    else (
      print_int prec;
      fibo_rec (v - 1) (prec + ante) prec)
  in
  fibo_rec n 1 0
;;

let a =
  print_int 6;
  4
;;
