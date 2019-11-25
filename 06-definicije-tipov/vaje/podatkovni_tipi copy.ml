(* ========== Vaja 3: Definicije Tipov  ========== *)
(* open List *)
(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 Pri modeliranju denarja ponavadi uporabljamo racionalna števila. Problemi se
 pojavijo, ko uvedemo različne valute.
 Oglejmo si dva pristopa k izboljšavi varnosti pri uporabi valut.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)
let denar = 4.3

type euro = Euro of float  
(* Euro je konstruktor *)

type dollar = Dollar of float

(*----------------------------------------------------------------------------*]
 Definirajte tipa [euro] in [dollar], kjer ima vsak od tipov zgolj en
 konstruktor, ki sprejme racionalno število.
 Nato napišite funkciji [euro_to_dollar] in [dollar_to_euro], ki primerno
 pretvarjata valuti (točne vrednosti pridobite na internetu ali pa si jih
 izmislite).

 Namig: Občudujte informativnost tipov funkcij.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # dollar_to_euro;;
 - : dollar -> euro = <fun>
 # dollar_to_euro (Dollar 0.5);;
 - : euro = Euro 0.4305
[*----------------------------------------------------------------------------*)

let dollar_to_euro_ratio = 1.00 /. 1.10

let dollar_to_euro (Dollar d) = Euro (d *. dollar_to_euro_ratio)
let euro_to_dollar (Euro e) = Dollar (e /. dollar_to_euro_ratio)

(*----------------------------------------------------------------------------*]
 Definirajte tip [currency] kot en vsotni tip z konstruktorji za jen, funt
 in švedsko krono. Nato napišite funkcijo [to_pound], ki primerno pretvori
 valuto tipa [currency] v funte.

 Namig: V tip dodajte še švicarske franke in se navdušite nad dejstvom, da vas
        Ocaml sam opozori, da je potrebno popraviti funkcijo [to_pound].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # to_pound (Yen 100.);;
 - : currency = Pound 0.007
[*----------------------------------------------------------------------------*)

type currency = 
  | Yen of float
	| Pound of float
	| Crown of float

let to_pound c = 
	match c with
	| Crown (cr) -> Pound (cr *. 0.3)
	| Yen (yn) -> Pound (yn *. 0.1)
	| p ->  p
 
 (*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 Želimo uporabljati sezname, ki hranijo tako cela števila kot tudi logične
 vrednosti. To bi lahko rešili tako da uvedemo nov tip, ki predstavlja celo
 število ali logično vrednost, v nadaljevanju pa bomo raje konstruirali nov tip
 seznamov.

 Spomnimo se, da lahko tip [list] predstavimo s konstruktorjem za prazen seznam
 [Nil] (oz. [] v Ocamlu) in pa konstruktorjem za člen [Cons(x, xs)] (oz.
 x :: xs v Ocamlu).
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

type int_bool_list =
	| Nil
	| Int of int * int_bool_list
	| Bool of bool * int_bool_list
	(* | Str of string * int_bool_list  lahko se dodamo ce bi hotl *)


(*----------------------------------------------------------------------------*]

 Definirajte tip [intbool_list] z konstruktorji za:
  1.) prazen seznam,
  2.) člen z celoštevilsko vrednostjo,
  3.) člen z logično vrednostjo.

 Nato napišite testni primer, ki bi predstavljal "[5; true; false; 7]".
[*----------------------------------------------------------------------------*)

let primer = Int (5, (Bool (true, (Bool (false, (Int (7, Nil)))))))

(*----------------------------------------------------------------------------*]
 Funkcija [intbool_map f_int f_bool ib_list] preslika vrednosti [ib_list] v nov
 [intbool_list] seznam, kjer na elementih uporabi primerno od funkcij [f_int]
 oz. [f_bool].
[*----------------------------------------------------------------------------*)

(* let rec intbool_map xs f_int f_bool= 
       let rec aux xs acc = 
              match xs with
              | Nil -> acc
              | Int x * xs' -> Bool (f_bool x, xs')
              | Bool x * xs' -> Int (f_int x, xs') *)

let rec intbool_map f_int f_bool = function (*argument za to funkcijo gre na konec*)
	| Nil -> Nil
	| Int (x, xs) -> Int (f_int x, intbool_map f_int f_bool xs)
	| Bool (y, ys) -> Bool (f_bool y, intbool_map f_int f_bool ys) 

(*----------------------------------------------------------------------------*]
 Funkcija [intbool_reverse] obrne vrstni red elementov [intbool_list] seznama.
 Funkcija je repno rekurzivna.
[*----------------------------------------------------------------------------*)

let rec intbool_reverse ib_list = 
	let rec intbool_reverse' acc = function
		| Nil -> acc
		| Int (x, xs) -> intbool_reverse' (Int (x, acc)) xs
		| Bool (y, ys) -> intbool_reverse' (Bool (y, acc)) ys
	in intbool_reverse' Nil ib_list 

let rec intbool_reverse_partial = 
	let rec intbool_reverse' acc = function
		| Nil -> acc
		| Int (x, xs) -> intbool_reverse' (Int (x, acc)) xs
		| Bool (y, ys) -> intbool_reverse' (Bool (y, acc)) ys
	in intbool_reverse' Nil

(*----------------------------------------------------------------------------*]
 Funkcija [intbool_separate ib_list] loči vrednosti [ib_list] v par [list]
 seznamov, kjer prvi vsebuje vse celoštevilske vrednosti, drugi pa vse logične
 vrednosti. Funkcija je repno rekurzivna in ohranja vrstni red elementov.
[*----------------------------------------------------------------------------*)

let rec intbool_separate ib_list = 
	let rec aux ib_list acc_int acc_bool = 
		match ib_list with
		| Nil -> (acc_int, acc_bool)
		| Int (x, xs) -> aux xs (x :: acc_int) acc_bool
		| Bool (y, ys) -> aux ys acc_int (y :: acc_bool)
	in aux (intbool_reverse ib_list) [] []

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 Določeni ste bili za vzdrževalca baze podatkov za svetovno priznano čarodejsko
 akademijo "Effemef". Vaša naloga je konstruirati sistem, ki bo omogočil
 pregledno hranjenje podatkov.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)


(*  *)


(*----------------------------------------------------------------------------*]
 Čarodeje razvrščamo glede na vrsto magije, ki se ji posvečajo. Definirajte tip
 [magic], ki loči med magijo ognja, magijo ledu in magijo arkane oz. fire,
 frost in arcane.

 Ko se čarodej zaposli na akademiji, se usmeri v zgodovino, poučevanje ali
 raziskovanje oz. historian, teacher in researcher. Definirajte tip
 [specialisation], ki loči med temi zaposlitvami.
[*----------------------------------------------------------------------------*)

type magic = Fire | Frost | Arcane (* | Nic *)

type specialisation = Historian | Teacher | Researcher

(*----------------------------------------------------------------------------*]
 Vsak od čarodejev začne kot začetnik, nato na neki točki postane študent,
 na koncu pa SE lahko tudi zaposli.
 Definirajte tip [status], ki določa ali je čarodej:
  a.) začetnik [Newbie],
  b.) študent [Student] (in kateri vrsti magije pripada in koliko časa študira),
  c.) zaposlen [Employed] (in vrsto magije in specializacijo).

 Nato definirajte zapisni tip [wizard] z poljem za ime in poljem za trenuten
 status.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # professor;;
 - : wizard = {name = "Matija"; status = Employed (Fire, Teacher)}
[*----------------------------------------------------------------------------*)

type status = 			(* to je samo tip *)
	| Newbie
	| Student of magic * int
	| Employed of magic * specialisation

type wizard = {name: string; status: status} (* to je zapisni tip*)

(* let vrsta_statusa carovnik = carovnik.status *)


(* napisi funkcijo, ki sprejme carovnika in pove njegovo vrsto carovnije *)

(* ta funkcija ne bi delala ce ne bi blo se konstruktorja Nic v tipu magic, ker nima
vsak status tudi podanege vrste magica *)

(* let vrsta_magic carovnik = 
	match carovnik with
	| {name:_; status = Newbie} -> Nic
	| {name:_; status = Student(carovnija,_)} -> carovnija
	| {name:_; status = Employed(carovnija,_)} -> carovnija *)


let izak = {name = "Izak"; status = Student(Frost, 2)}
let professor = {name = "Matija"; status = Employed (Fire, Teacher)}
(*----------------------------------------------------------------------------*]
 Želimo prešteti koliko uporabnikov posamezne od vrst magije imamo na akademiji.
 Definirajte zapisni tip [magic_counter], ki v posameznem polju hrani število
 uporabnikov magije.
 Nato definirajte funkcijo [update counter magic], ki vrne nov števec s
 posodobljenim poljem glede na vrednost [magic].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # update {fire = 1; frost = 1; arcane = 1} Arcane;;
 - : magic_counter = {fire = 1; frost = 1; arcane = 2}
[*----------------------------------------------------------------------------*)

type magic_counter = {fire: int; frost: int; arcane: int} (* zapisni tip je produkt imenovanih faktorjev *)

let update counter magic = 
	match magic with
	| Fire -> {fire = counter.fire + 1; frost = counter.frost; arcane = counter.arcane}
	| Frost -> {fire = counter.fire; frost = counter.frost + 1; arcane = counter.arcane}
	| Arcane -> {fire = counter.fire; frost = counter.frost; arcane = counter.arcane + 1}


let update_krajsi counter = function
	| Fire -> {counter with fire = counter.fire + 1}
	| Frost -> {counter with frost = counter.frost + 1}
	| Arcane -> {counter with arcane = counter.arcane + 1}

let update_zmesan counter magic = 
	match magic with
	| Fire -> {{counter with fire = counter.fire + 1} with frost = {counter with fire = counter.fire + 1}.frost + 100}
	| Frost -> {counter with frost = counter.frost + 1}
	| Arcane -> 
		let counter' = {counter with arcane = counter.arcane + 1} in
		{counter' with frost = counter'.frost + 500} (* ce hocem spreminjat vec faktorjev v produktu (npr. v magic_counter) *)

let stevec_carovnije = {fire = 0; frost = 0; arcane = 0} 

(*----------------------------------------------------------------------------*]
 Funkcija [count_magic] sprejme seznam čarodejev in vrne števec uporabnikov
 različnih vrst magij.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # count_magic [professor; professor; professor];;
 - : magic_counter = {fire = 3; frost = 0; arcane = 0}
[*----------------------------------------------------------------------------*)

let count_magic sez = 
	let prazen_counter = {fire = 0; frost = 0; arcane = 0} in
	let rec aux sez counter = 
		match sez with
		| [] -> counter
		| x :: xs -> 
			match x.status with
			| Newbie -> aux xs counter
			| Student(carovnija, _ ) -> aux xs (update counter carovnija) 
			| Employed(carovnija, _ ) -> aux xs (update counter carovnija)
	in aux sez prazen_counter



let count_magic_2 wizard_list =
  let rec count counter = function
    | [] -> counter
    | {name; status} :: wizards -> (
        match status with
        | Newbie -> count counter wizards
        | Student (magic, _) -> count (update counter magic) wizards
        | Employed (magic, _) -> count (update counter magic) wizards)
  in count {fire = 0; frost = 0; arcane = 0} wizard_list


(* count_magic [proffesor; proffesor; izak; izak; proffesor; izak; izak];;*)

(*----------------------------------------------------------------------------*]
 Želimo poiskati primernega kandidata za delovni razpis. Študent lahko postane
 zgodovinar po vsaj treh letih študija, raziskovalec po vsaj štirih letih
 študija in učitelj po vsaj petih letih študija.
 Funkcija [find_candidate magic specialisation wizard_list] poišče prvega
 primernega kandidata na seznamu čarodejev in vrne njegovo ime, čim ustreza
 zahtevam za [specialisation] in študira vrsto [magic]. V primeru, da ni
 primernega kandidata, funkcija vrne [None].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # let jaina = {name = "Jaina"; status = Student (Frost, 4)};;
 # find_candidate Frost Researcher [professor; jaina];;
 - : string option = Some "Jaina"
[*----------------------------------------------------------------------------*)

let rec find_candidate magic specialisation wizard_list = 
	match wizard_list with
	| [] -> None
	| x :: xs ->
		match x.status with
		| Employed ( _ , _) -> find_candidate magic specialisation xs
		| Newbie -> find_candidate magic specialisation xs
		| Student (carovnija, l) -> 
			if carovnija != magic then find_candidate magic specialisation xs
			else
				match specialisation with
				| Historian -> if l >= 3 then Some x.name else find_candidate magic specialisation xs
				| Researcher -> if l >= 4 then Some x.name else find_candidate magic specialisation xs
				| Teacher -> if l >= 5 then Some x.name else find_candidate magic specialisation xs


let find_candidate_2 magic specialisation wizard_list =
  let year =
    match specialisation with
    | Historian -> 3
    | Researcher -> 4
    | Teacher -> 5
  in
  let rec search = function
    | [] -> None
    | {name; status} :: wizards ->
        match status with
        | Student (m, y) when m = magic && y >= year -> Some name
        | _ -> search wizards
  in
  search wizard_list