(* 1. naloga *)

(* a *)

let odstej_trojici (x,y,z) (a,b,c) = (x-a, y-b, z-c)

(* b *)

let max_rezultat_do_n f n =
	let rec aux maximum = function
		| 0 -> maximum
		| m when f m > maximum -> aux (f m) (m - 1)
		| m -> aux maximum (m - 1)
	in aux (f n) n

let test_f = fun x -> - x * x

let a = max_rezultat_do_n test_f 10

(* c *)

let pocisti_seznam (sez : 'a option list) = 
	let rec aux acc = function
		| [] -> acc
		| None :: xs -> aux acc xs
		| (Some x) :: xs -> aux (x :: acc) xs
	in aux [] (List.rev sez)  

let sez = [None; Some 4; None; Some 2; Some 0]

(* d *)

let pi1 (x,y) = x
let pi2 (x,y) = y

let preveri_urejenost sez =
	let rec loci xs sodi lihi = match xs with
		| [] -> sodi, lihi
		| s :: xs' when s mod 2 = 0 -> loci xs' (s :: sodi) lihi
		| l :: xs' -> loci xs' sodi (l :: lihi)
	in 
	let rec preveri_narascanje = function
		| [] | [_] -> true
		| x :: y :: xs' when x <= y -> preveri_narascanje (y :: xs')
		| _ -> false
	in
	let sortirani = loci sez [] []
	in
	(List.rev (pi1 sortirani)) |> preveri_narascanje && (pi2 sortirani) |> preveri_narascanje
	
let rec loci xs sodi lihi = match xs with
		| [] -> sodi, lihi
		| s :: xs' when s mod 2 = 0 -> loci xs' (s :: sodi) lihi
		| l :: xs' -> loci xs' sodi (l :: lihi)

let sez = [5;2;4;1;6]
let sez_ = [3;2;4;5;6]

(* 2. naloga *)

type 'a gnezdenje =
	| Element of 'a
	| Podseznam of 'a gnezdenje list

(* a *)

let gnezdenje_primer =
Podseznam([
	Element(1);
	Element(2);
	Podseznam([
		Element(3);
		Podseznam([
			Element(4)
		]);
		Podseznam([])
	]);
	Podseznam([
		Element(5)
	])
])

let gnezdenje_primer' = 
[
	Element(1);
	Element(2);
	Podseznam([
		Element(3);
		Podseznam([
			Element(4)
		]);
		Podseznam([])
	]);
	Podseznam([
		Element(5)
	])
]

(* b *)

let najvecja_globina' g_sez = 
	let rec aux g_sez = match g_sez with
	| Element(_) -> 0
	| Podseznam([]) -> 1
	| Podseznam(x :: xs) -> 1 + (List.fold_left max (aux x) (List.map aux xs))
	in aux g_sez

let najvecja_globina (g_sez : 'a gnezdenje list) = 
	let globine = List.map najvecja_globina' g_sez in
	match globine with
	| [] -> 1
	| x :: xs -> 1 + List.fold_left max x xs

let g_test = Podseznam([Podseznam([Podseznam([Podseznam([Element(4)])])]);Podseznam([Podseznam([Podseznam([Podseznam([Element(4)])])])])])

(* c *)

let preslikaj' f g_sez =
	let rec aux f g_sez = match g_sez with
	| Element(x) -> Element(f x)
	| Podseznam([]) -> Podseznam([])
	| Podseznam(x :: xs) -> Podseznam((aux f x) :: (List.map (aux f) xs))
	in aux f g_sez

let preslikaj f (g_sez : 'a gnezdenje list) = List.map (preslikaj' f) g_sez 

let f = fun x -> x + 1

(* d *)

let splosci g_sez = 
	let rec aux g_sez = match g_sez with
	| [] -> []
	| Element(a) :: xs -> a :: aux xs
	| Podseznam(ys) :: xs -> (aux ys) @ (aux xs)
	in aux g_sez


(* vrjetno je tr, testiral na loger_list_gen 10000 in je delalo *)
let splosci_tr_mogoce g_sez =
	let rec aux g_sez acc = match g_sez with
	| [] -> acc
	| Element(a) :: xs -> aux xs (a :: acc)
	| Podseznam(ys) :: xs -> aux xs (List.rev_append (List.rev (aux ys [])) acc) (* zadnji oklepaj je enak (aux ys []) @ acc *)
	in (aux g_sez []) |> List.rev

let long_list_gen n =
	let rec aux n acc = match n with
	| 0 -> acc
	| n -> aux (n - 1) ((Podseznam([Element(Random.int 100)])) :: acc)
	in aux n []

let longer_list_gen n = 
	let rec aux n acc = match n with
	| 0 -> acc
	| n -> aux (n - 1) (Podseznam(long_list_gen n) :: acc)
	in (aux n []) |> List.rev
	
(* 

List.rev l1 @ l2 = List.rev_append l1 l2 
l1 @ l2 = List.rev_append (List.rev l1) l2

(aux ys []) @ acc = List.rev_append (List.rev (aux ys [])) acc

*)

(* e *)

(* state :
	0 pomeni prejsnji element je bil kostruktorja Element of 'a
	1 pomeni prejsnji element je bil kostruktorja Podseznam of 'a list
 *)

let alternirajoci_konstruktorji' g_sez = 

	let rec aux xs state = match xs with
		| [] -> true
		| Element(_) :: xs' -> 
			if state = 0 then false else aux xs' 0
		| Podseznam(_) :: xs' ->
			if state = 1 then false else aux xs' 1 
	in 
	let loci g_sez = match g_sez with
	| [] -> true
	| Element(a) :: xs -> aux xs 0
	| Podseznam(ys) :: xs -> aux xs 1
	in loci g_sez


let rec alt xs state = match xs with
		| [] -> true
		| Element(_) :: xs' -> 
			if state = 0 then false else alt xs' 0
		| Podseznam(_) :: xs' ->
			if state = 1 then false else alt xs' 1 

let alternirajoci_konstruktorji = function
	| [] -> true
	| Element(a) :: xs -> alt xs 0
	| Podseznam(ys) :: xs -> alt xs 1

let alt_test = [Element(1); Podseznam([]); Element(1); Podseznam([])] 
let alt_test2 = [Podseznam([]); Element(1); Podseznam([])] 

(* f *)

(* ne-repno rekurzivna resitev (zaradi splosci) *)
let zlozi_preko_gnezdenja_not_tr f a g_sez = List.fold_left f a (splosci g_sez)

(* fold_left f a [b_1; b_2 ... ; bn] = f (... (f (f a b_1) b_2) ...) b_n  *)

(* 
(* poskus repno rekurzivne resitve *)
let zlozi_preko_gnezdenja f a g_sez =
	let rec aux f a g_sez acc = match g_sez with
	(* v acc bo najbolj zadnji rezultat (f a g_i) *)
	| [] -> acc
	| Element(x) :: xs -> aux f acc xs (f acc x)
	| Podseznam(ys) :: xs -> aux f acc (ys @ xs) (f acc )
	 *)

(* tr je odvisna od splosci_tr_mogoce *)
let zlozi_preko_gnezdenja_tr_mogoce f a g_sez = List.fold_left f a (splosci_tr_mogoce g_sez)