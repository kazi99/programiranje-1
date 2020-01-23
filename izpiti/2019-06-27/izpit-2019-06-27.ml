(* 1. naloga *)

type complex = { re: float; im: float }

(* a *)

let complex_add {re = a ; im = b} {re = c ; im = d} = {re = a +. c ; im = b +. d}
let complex_add' x y = { re = x.re +. y.re ; im = x.im +. y.im }


(* b *)

let complex_conjugate {re = a ; im = b} = {re = a ; im = -.b}
let complex_conjugate' {re ; im} = {re ; im = -. im}

(* c *)

let rec list_apply_either pred f g = function
  | [] -> []
  | x :: xs when pred x -> (f x) :: list_apply_either pred f g xs
  | x :: xs -> (g x) :: list_apply_either pred f g xs

(* d *)

let power x n = 
  let rec aux x n acc = match n with
  | 0 -> acc
  | n -> aux x (n - 1) (x * acc)
  in aux x n 1

let eval_poly koef x =  
  let rec aux p x acc st = match p with
  | [] -> acc
  | k :: ps -> aux ps x (acc + k * power x st) (st + 1)
  in aux koef x 0 0

let poly = [3; -2; 0; 1]

(* 2. naloga *)

type najemnik = string

type vrt = 
  | Obdelovan of najemnik
  | Oddan of najemnik * (vrt * vrt list)
  | Prost

(* a *)

let vrt_primer = 
Oddan ("Kovalevskaya", (
  Obdelovan "Galois", [
    Obdelovan "Lagrange";
    Prost
  ])
)

(* b *)

let obdelovalec_vrta vrt = match vrt with
  | Obdelovan a -> Some a
  | Oddan _ -> None
  | Prost -> None 

(* c *)

let globina_oddajanja vrt = 
  let rec aux acc vrt = match vrt with
  | Prost -> acc
  | Obdelovan _ -> acc
  | Oddan (_,(b,xs)) -> 1 + List.fold_left max (aux acc b) (List.map (aux acc) xs)
  in aux 0 vrt

let v = 
Oddan("a",(
  Oddan("b",(Obdelovan "c", [Prost])) , [
    Oddan("d",(Obdelovan "e", [
      Oddan("f",(Obdelovan "g", [Prost]))
      ]));
    Oddan("h",(Obdelovan "i", [Prost]))
    ]
  )
)

let pod_v = Oddan("a", (Obdelovan "b", Prost :: []))

let v' = Oddan(
  "c", (
    pod_v, [
      Oddan(
        "d", (
          pod_v, [
            Oddan(
              "e", (
                pod_v, [Prost]
              )
            )
          ]
        )
      )
    ]
  )
)

let v'' = Oddan (
  "c", (
    pod_v, [
      Oddan (
        "d", (
          pod_v, [Prost]
        )
      )
    ]
  )
)

(* d *)

let v_uporabi vrt = 
  let rec aux stanje vrt = match vrt with
  | Prost -> stanje
  | Obdelovan _ -> true
  | Oddan (_, (podvrt, podvrtovi)) -> List.fold_left (||) (aux stanje podvrt) (List.map (aux stanje) podvrtovi)
  in aux false vrt

(* e *)

let vsi_najemniki vrt = 
  let rec aux najemniki vrt = match vrt with
  | Prost -> najemniki
  | Obdelovan x -> x :: najemniki
  | Oddan (x, (podvrt, podvrtovi)) -> x :: List.fold_left (@) (aux najemniki podvrt) (List.map (aux najemniki) podvrtovi)
  in aux [] vrt

(* f *)

let vsi_obdelovalci vrt =
  let rec aux obdelovalci = function
  | Prost -> obdelovalci
  | Obdelovan x -> x :: obdelovalci
  | Oddan (_, (podvrt, podvrtovi)) -> List.fold_left (@) (aux obdelovalci podvrt) (List.map (aux obdelovalci) podvrtovi)
  in aux [] vrt