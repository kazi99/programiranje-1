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

(* b) *)

let rec vstavi t veriga = 
	match veriga with
	| Ostalo xs -> Ostalo (t :: xs)
	| Filter (f, xs, veriga') -> if f t then Filter (f, t :: xs, veriga') else Filter (f, xs, vstavi t veriga')

(* c) *)

(* let rec poisci t veriga =
	match veriga with
	| Ostalo xs -> List.exists t xs
	| Filter (f, xs, veriga') -> if f t then List.exists t xs else poisci t veriga' *)

(* d) *)

(* let izprazni_filtre veriga = 
	let rec aux veriga acc_sez =
		match veriga with
		| Ostalo xs -> (Ostalo [], xs @ acc_sez) 
		| Filter (f, xs, veriga') -> (Filter(f, [], aux veriga' (xs @ acc_sez)), xs @ acc_sez)
	in aux veriga [] *)

let izprazni_filtre veriga = 
	let rec aux acc = function
	| Ostalo xs -> 

let rec izprazni_filtre_brez_sez veriga =
	match veriga with
	| Ostalo xs -> Ostalo []
	| Filter (f, xs, v) -> Filter (f, [], izprazni_filtre_brez_sez v)
