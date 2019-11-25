(* =================== *)
(* 1. naloga: funkcije *)
(* =================== *)

(* 1.1) Definirajte funkcijo, ki vzame tri cela števila ter vrne njihov produkt.
   Primer: /zmnozi 2 3 4 = 24/ *)
 let zmnozi a b c = a * b * c

(* 1.2) Definirajte funkcijo, ki vzame celo število x in celo število k, ter
   vrne vrednost izraza x^3 + k.
   Primer: /afin_kub 2 1 = 9/ *)
 let afin_kub x k = x * x * x + k

(* 1.3) Definirajte funkcijo, ki vzame seznam in izračuna seznam vrednosti funkcije
   f(x) = x^3 + 2 za elemente vhodnega seznama.
   Primer: /vse_kubiraj_in_pristej_dva [1; 2; 3] = [3; 10; 29]/ *)
 let rec vse_kubiraj_in_pristej_dva = function
    | [] -> []
    | x :: xs -> afin_kub x 2 :: vse_kubiraj_in_pristej_dva xs

(* 1.4) Definirajte funkcijo, ki varno vrne zadnji element seznama v primeru,
   da seznam ni prazen. Uporabite tip option.
   Primer: /zadnji_element [1; 2; 3] = Some 3/ *)

let reverse xs =
  let rec aux xs acc = 
    match xs with
    | [] -> acc
    | x :: xs' -> aux xs' (x :: acc)
  in aux xs [] 

 let zadnji_element sez = 
    match reverse sez with
    | [] -> None
    | x :: _ -> Some x

(* 1.5) Definirajte funkcijo, ki izračuna n-to Fibonaccijevo število.
   Pri tem upoštevamo začetna pogoja /fibonacci 0 = 1/ in /fibonacci 1 = 1/.
   Primer: /fibonacci 20 = 10946/ *)

let fst_element = function
    | [] -> failwith "Error"
    | x :: _ -> x

let snd_element = function
    | [] | [_] -> failwith "Error snd" 
    | _ :: x :: _ -> x

 let rec fibonacci n = 
    let rec aux n acc = 
        match n with
        | 0 -> acc
        | 1 -> acc
        | n -> aux (n - 1) ((fst_element acc + snd_element acc) :: acc)
    in fst_element (aux n [1;1])


(* ======================================= *)
(* 2. naloga: podatkovni tipi in rekurzija *)
(* ======================================= *)

(* 2.1) Rožno drevo je drevo, v katerem ima vsak koren poljubno mnogo otrok,
   ki so zopet rožna drevesa. Rožna drevesa predstavimo s parametričnim
   tipom /'a drevo/ z enim konstruktorjem, ki sprejme:
   - vrednost (koren) tipa /'a/ in
   - seznam (gozd) dreves tipa /'a drevo/. *)
type 'a drevo = Drevo of 'a * 'a drevo list

(* 2.2) Definirajte naslednja rožna drevesa:

   t = 1,  t' =  2   ,      t'' =  3
                / \               /| \
               t  t              / |  \
                               -1  t'  0

 *)

let t = Drevo (1, [])
let t' = Drevo (2, [t;t])
let t'' = Drevo (3, [Drevo (-1, []); t'; Drevo(0, [])])

(* 2.3) Definirajte funkcijo, ki preveri ali je dano rožno drevo list drevesa,
   torej ima prazen gozd poddreves. *)

let je_list (Drevo(_, sez)) = sez = []

(* 2.4) Definirajte funkcijo, ki preveri, ali drevo celih števil vsebuje zgolj pozitivna števila. *)

let rec vsa_pozitivna' (Drevo(n, sez)) = 
    if n <= 0 then false
    else match sez with
        | [] -> true
        | x :: xs -> 
        let rec vsa_poz_sez = function
            | [] -> true
            | t :: ts -> (vsa_pozitivna' t) && (vsa_poz_sez ts)
        in 
        vsa_poz_sez (x :: xs)
        

let rec vsa_pozitivna tree = 
    match tree with
    | Drevo (n, []) -> n > 0
    | Drevo (n, t :: ts) ->
        if n <= 0 then false
        else
        let rec vsa_poz_sez = function
            | [] -> true
            | x :: xs -> 
                if not (vsa_pozitivna x) then false
                else
                vsa_poz_sez xs
        in vsa_poz_sez ts


(* 2.5) Definirajte funkcijo, ki izračuna največjo širino rožnega drevesa, torej največjo dolžino
   gozda, ki se pojavi v kateremkoli vozlišču rožnega drevesa. *)

let dolzina_sez sez =
    let rec aux sez acc =
        match sez with
        | [] -> acc
        | x :: xs -> aux xs (1+acc)
    in aux sez 0

let starting_element = function
  | [] -> failwith "ERROR"
  | x :: _ -> x

let largest xs = 
  let rec largest' xs big = 
    match xs with
    | [] -> big
    | x :: xs' -> if x >= big then largest' xs' x else largest' xs' big
  in largest' xs (starting_element xs)


let rec po_sez sez acc f = 
    match sez with
    | [] -> acc
    | x :: xs -> (f x acc) @ po_sez xs acc f

    
let sirina_drevesa' tree =
    let rec aux tree acc = 
        match tree with
        | Drevo(_,[]) -> acc
        | Drevo(_, x :: xs) -> (dolzina_sez (x :: xs)) :: po_sez xs acc aux
    in aux tree []
    
let sirina_drevesa tree = largest (sirina_drevesa' tree)


(* 2.6) Definirajte funkcijo, ki sestavi (poljubno) rožno drevo globine n.
   Vrednosti v korenih so poljubne. *)
(* let globoko_drevo = failwith "dopolni me" *)

(* 2.7) Definirajte funkcijo, ki pretvori rožno drevo v seznam. Vrstni red vrednosti v seznamu
   pri tem ni pomemben.
   Primer: /drevo_v_seznam t'' = [3; -1; 2; 1; 1; 0]/ (ali katerakoli permutacija [3; -1; 2; 1; 1; 0])

   Če želite vse točke, mora biti funkcija repno rekurzivna.

   Opomba: kot ste videli na vajah, nekatere funkcije iz modula List,
   na primer List.map, niso repno rekurzivne, zato se jim raje
   izognite. *)

let drevo_v_seznam tree =
    let rec aux tree acc =
        match tree with
        | Drevo(a,[]) -> a :: acc
        | Drevo(a, sez) -> a :: po_sez sez acc aux
    in aux tree []

