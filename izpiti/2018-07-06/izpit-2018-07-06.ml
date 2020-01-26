(* 1.naloga *)

(* a *)

let uporabi f a = f a

(* b *)

let ibaropu a f = f a

(* c *)

(* zgleda tr in deluje na primerih dolzine 100000 *)
let zacetnih n xs =
	if List.length xs >= n then 
  	let rec aux n xs acc = match xs with
		| [] -> List.rev acc
		| x :: xs' when n > 0 -> aux (n - 1) xs' (x :: acc)
		| x :: xs' -> acc
		in Some (aux n xs [])
	else None

let rec long_list n acc = match n with
	| 0 -> acc
	| n -> long_list (n - 1) (1 :: acc)

(* 2. naloga *)

type 'a neprazen_sez = 
	| Konec of 'a
	| Sestavljen of 'a * 'a neprazen_sez

(* a *)

let prvi = function
	| Konec a -> a
	| Sestavljen (a, _) -> a

let rec zadnji nepr_sez = match nepr_sez with
	| Konec a -> a
	| Sestavljen (_, nepr_podsez) -> zadnji nepr_podsez

(* b *)

let rec dolzina = function
	| Konec _ -> 1
	| Sestavljen (_, nepr_podsez) -> 1 + dolzina nepr_podsez

(* c *)

let pretvori_v_seznam nepr_sez = 
	let rec aux nepr_sez acc = match nepr_sez with
	| Konec a -> a :: acc
	| Sestavljen (a, nepr_podsez) -> aux nepr_podsez (a :: acc)
	in List.rev (aux nepr_sez [])

let nepr_sez = Sestavljen(1, Sestavljen (2, Sestavljen (5, Sestavljen (4, Konec 0))))

(* d *)

(* 
	f ( ... (f (f x a_1) a_2) ... ) a_n
 *)

let zlozi_lazy f x nepr_sez = List.fold_left f x (pretvori_v_seznam nepr_sez)

let f = fun x y -> y + x

let rec zlozi f x = function
	| Konec a -> f x a
	| Sestavljen (a, nepr_podsez) -> zlozi f (f x a) nepr_podsez