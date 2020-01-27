(* 1. naloga *)

(* a *)

let rec izpisi_vsa_stevila = function
  | [] -> ()
	| x :: xs -> 
		print_int x;
		izpisi_vsa_stevila xs

(* b *)

let map2_opt f xs ys  =
	let rec aux f xs ys acc = match xs, ys with
		| [], [] -> acc
		| [], y :: ys' -> []
		| x :: xs', [] -> []
		| x :: xs', y :: ys' -> aux f xs' ys' ((f x y) :: acc )
	in
	let output = aux f xs ys []
	in
	if output = [] then None
	else Some (List.rev output)

let a = map2_opt (+) [1;2;3] [7;5;3]
let b = map2_opt (+) [1;2;3] [7;5]

let map2_opt' f xs ys  =
	let rec aux f xs ys acc = match xs, ys with
		| [], [] -> Some (List.rev acc)
		| [], y :: ys' -> None
		| x :: xs', [] -> None
		| x :: xs', y :: ys' -> aux f xs' ys' ((f x y) :: acc )
	in aux f xs ys []

let a' = map2_opt' (+) [1;2;3] [7;5;3]
let b' = map2_opt' (+) [1;2;3] [7;5]

(* 2. naloga *)

(* a *)

type filter_tree = 
	| Node of filter_tree * int * filter_tree
	| Box of int list

let primer = Node (
	Node (
		Box ([1]),
		5,
		Box ([])
	),
	10,
	Node (
		Box ([]),
		15,
		Box ([19;20])
	)
)

(* b *)

let rec vstavi tree n = match tree with
	| Box (xs) -> Box(List.sort compare (n :: xs))
	| Node (l, k, d) when n <= k -> Node (vstavi l n, k, d)
	| Node (l, k, d) -> Node (l, k, vstavi d n)

(* c *)

(* 
List.fold_left vstavi tree sez
vstavi (... (vstavi (vstavi tree a_1) a_2) ... ) a_n

List.fold_right vstavi sez tree
vstavi a_1 (vstavi a_2 (... (vstavi a_n tree) ...))
*)

let vstavi_seznam sez tree = List.fold_left vstavi tree sez

(*
let vstavi_seznam' l tree = List.fold_right vstavi l tree 

ce bi imel funkcijo `vstavi` napisano kot vstavi : int -> filter_tree -> filter_tree
*)

(* d *)

let naberi_skatle tree =
	let rec aux tree acc = match tree with
		| Box (xs) -> xs @ acc
		| Node (l, k, d) -> aux l (aux d acc)
	in aux tree []

let rec izprazni_skatle = function
	| Box (_) -> Box ([])
	| Node (l, k, d) -> Node (izprazni_skatle l, k, izprazni_skatle d)

(* obcutljivo na urejenost boxov, amapk ok zaenkrat *)
let pravilno tree = (tree = vstavi_seznam (naberi_skatle tree) (izprazni_skatle tree))
	(* let test_tree = vstavi_seznam (naberi_skatle tree) (izprazni_skatle tree)
	in match tree, test_tree with
		| Box(xs), Box(test) ->   *)

let test1 = Node (
	Box ([1;2]),
	5,
	Box ([7])
)

let test2 = Node (
	Box ([1]),
	5,
	Box ([2;7])
)