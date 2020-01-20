(* izpit 2018-08-28*)

(* 1. naloga *)
(* a) *)

let razlika_kvadratov a b = (a + b) * (a + b) - (a * a + b * b)

(* b) *)

let uporabi_na_paru f (a, b) = (f a, f b)

(* c) *)

let ponovi_seznam n sez = 
	let rec aux n sez acc =
		match n with
		| 0 -> acc
		| n -> aux (n - 1) sez (sez @ acc)
	in
	aux n sez []

(* d) *)

let razdeli sez = 
	let rec aux sez (neg, not_neg) =
	match sez with
	| [] -> (neg, not_neg)
	| x :: xs when x < 0 -> aux xs (x :: neg, not_neg)
	| x :: xs -> aux xs (neg, x :: not_neg)
	in aux sez ([], [])

(* 2. naloga *)

type 'a tree = 
	| Empty
	| Node of 'a tree * 'a * 'a tree

(* let max_dolzina sez1 sez2 =
	if List.length sez1 > List.length then sez1
	else sez2 *)

let node = function
	| Empty -> None
	| Node (_, x, _) -> Some x

(* let monotona_pot tree = 
	match tree with
	| Empty ->
	| Node(l, x, r) -> max_dolzina (monotona_pot l) (monotona_pot r) *)
		

(* 3. naloga *)

type 'a veriga = 
	| Filter of ('a -> bool) * 'a list * 'a veriga
	| Ostalo of 'a list


(* fun x -> x < 10 *)

(* a) *)

let test_f x = if x >= 0 && x < 10 then true else false
let test = Filter (test_f, [], Ostalo [])

let f_0 = fun x -> x < 0
let f_10 = fun x -> x < 10

let test' = Filter (f_0, [], Filter (f_10, [], Ostalo []))

(* b) *)

let rec vstavi t veriga = 
	match veriga with
	| Ostalo xs -> Ostalo (t :: xs)
	| Filter (f, xs, veriga') -> if f t then Filter (f, t :: xs, veriga') else Filter (f, xs, vstavi t veriga')

let vst_test = vstavi (-5) (vstavi (7) (vstavi 100 (vstavi (-7) (vstavi 2 test'))))

(* c) *)

let rec poisci t veriga =
	match veriga with
	| Ostalo xs -> List.mem t xs
	| Filter (f, xs, veriga') -> if f t then List.mem t xs else poisci t veriga'

(* d) *)

let pi_1 (x, y) = x
let pi_2 (x, y) = y

let izprazni (veriga : 'a veriga) =
	let rec aux veriga = match veriga with
    | Ostalo xs -> Ostalo []
    | Filter(f, xs, veriga') -> Filter(f, [], aux veriga')
  in aux veriga 

let naberi (veriga : 'a veriga) =
  let rec aux veriga acc = match veriga with
    | Ostalo xs -> xs @ acc
    | Filter(_, xs, veriga') -> xs @ (aux veriga' acc)
  in aux veriga []

let izprazni_filtre (veriga : 'a veriga) = izprazni veriga, naberi veriga

let izprazni_filtre_better veriga =
  let rec aux veriga acc = match veriga with
    | Ostalo xs -> Ostalo [], xs @ acc
    | Filter(f, xs, veriga') -> (Filter(f, [], pi_1 (aux veriga' [])), xs @ (pi_2 (aux veriga' acc)))
  in aux veriga []

(* e) *)

let dodaj_filter f veriga = 
  let elementi = naberi veriga in
  let izpr_veriga = izprazni veriga in
  let nova_veriga = Filter(f, [], izpr_veriga) in
  let rec nazaj_vstavi sez veriga = match sez with
    | [] -> veriga
    | x :: xs -> nazaj_vstavi xs (vstavi x veriga)
  in
  nazaj_vstavi elementi nova_veriga

let dodaj_filter_short f veriga =
  let rec aux sez veriga = match List.rev sez with
    | [] -> veriga
    | x :: xs -> aux xs (vstavi x veriga)
  in
  aux (naberi veriga) (Filter(f, [], izprazni veriga))

let je_sod = fun x -> (x mod 2 = 0)