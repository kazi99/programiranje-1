(* 1.naloga *)

(* a *)

let option_sum x y = match x, y with
	| None, None -> None
	| None, Some _ -> None
	| Some _, None -> None
	| Some a, Some b -> Some (a + b)

(* b *)

let twostep_map f g h x = match (f x) with
	| (a, b) -> (g a, h b)

let f = fun x -> (2 * x , 3 * x)
let g = fun x -> - x
let h = fun x -> 1000 * x

(* c *)

let ponovi x n = 
	let rec aux x n acc =
		if n <= 0 then acc
		else aux x (n - 1) (x :: acc)
	in aux x n []

let function_repeat f xs = 
	let rec aux f xs acc = match xs with
		| [] -> List.rev acc
		| x :: xs' -> aux f xs' (List.rev_append (ponovi x (f x)) acc) 
	in aux f xs []

let f' = fun x -> 2 * x

(* ponovi je repno rek. funkcija, in List.rev tudi. 
Ti dve funkciji pa uporabimo v funkciji function_repeat ki pa je res rep. rek.,
saj sproti racuna seznam ponovitev, ki ga shranjuje v acc
 in se zato rekurzivni klici ne nalagajo na stack *)

(* d *)

let iterate f pogoj a = 
	let rec aux f pogoj a acc =
		if pogoj acc = false then aux f pogoj a (f acc)
		else acc
	in aux f pogoj a a 
	
(* pogoj je prvi ki da true in tega vrni *)
let pogoj = fun x -> x >= 100



(* ----------------------------------- *)
(* 2. naloga *)

(* a *)

type 'a improvied_list =
	| Prazen
	| Sestavljen of 'a array * 'a improvied_list

let test = Sestavljen (
	[|1;2;20|], Sestavljen (
		[|17;19;20;30|], Sestavljen (
			[|100|], Prazen)))

(* b *)

let count ls =
	let rec aux sez acc = match sez with
		| Prazen -> acc
		| Sestavljen (podsez, i_podsez) -> aux i_podsez (acc + Array.length podsez)
	in aux ls 0

(* c *)

let nth n ls = 
	if count ls <= n then None
	else 
		let rec aux n ls = match ls with
			| Prazen -> None
			| Sestavljen (xs, i_podsez) -> 
				if (Array.length xs) > n then Some xs.(n)
				else aux (n - Array.length xs) i_podsez
		in aux n ls

(* d *)

(* treba je pogledat da so vsi podsez urejeni in da je zadnji v prejsnjem < kot prvi v naslednjem *)

let rec is_sorted_list x = match x with
  | [] -> true
  | h::[] -> true
  | h::h2::t -> if h <= h2 then is_sorted_list (h2::t) else false;;

(* nima linearne casnovne zahtevnosti *)
let is_sorted ls = 
	let rec aux ls = match ls with
		| Prazen -> true
		| Sestavljen ([||], podsez) -> aux podsez
		| Sestavljen (sez, Prazen) -> is_sorted_list (Array.to_list (sez))
		| Sestavljen (sez, podsez) -> 
			if is_sorted_list (Array.to_list (sez)) && Some (sez.((Array.length sez) - 1)) < nth 0 podsez then
				aux podsez
			else false
	in aux ls 


let test2 = Sestavljen([|1;2|],Sestavljen([|3;5|], Prazen))

(* e za pol *)

