(* =================== *)
(* 1. naloga: funkcije *)
(* =================== *)

(* 1.1) Definirajte funkcijo, ki vzame par in zamenja komponenti para.
   Primer: /obrni (2, 4) = (4, 2)/ *)
 let obrni (x, y) = (y, x)

(* 1.2) Definirajte funkcijo, ki vzame par p in vrednost x in zamenja drugo
   komponento para p z x.
   Primer: /zamenjaj_drugo (2, 4) 7 = (2, 7)/ *)
 let zamenjaj_drugo (x,y) z = (x,z)

(* 1.3) Definirajte funkcijo, ki vzame seznam parov in izračuna nov seznam parov,
   ki imajo drugo komponento zamenjano z 42.
   Primer: /vsem_zamenjaj_drugo_z_42 [(12, 1); (2, 4)] = [(12, 42); (2, 42)]/ *)

 let rec vsem_zamenjaj_drugo_z_42 = function
    | [] -> []
    | (a,b) :: xs -> (zamenjaj_drugo (a,b) 42) :: (vsem_zamenjaj_drugo_z_42 xs)

(* 1.4) Definirajte funkcijo, ki varno vrne glavo seznama v primeru, ko seznam ni prazen.
   Uporabite tip option.
   Primer: /glava [1; 2; 3] = Some 1/ *)

 let glava sez =
    match sez with
    | [] -> None
    | x :: xs -> Some x

(* 1.5) Definirajte funkcijo, vzame funkcijo (f: 'a -> 'b), neko vrednost (x : 'a) in
   celo število n. Funkcija naj vrne vrednost, ki jo dobimo če f n-krat uporabimo na x,
   torej f (f ... (f x)...).
   Primer: /uporabi_veckrat succ 0 420 = 420/ *)

 let rec uporabi_veckrat f x n =
    match n with
    | 0 -> x
    | n -> uporabi_veckrat f (f x) (n - 1)

(* ======================================= *)
(* 2. naloga: podatkovni tipi in rekurzija *)
(* ======================================= *)

(* 2.1) Rožno drevo je drevo, v katerem ima vsak koren poljubno mnogo otrok,
   ki so zopet rožna drevesa. Rožna drevesa predstavimo s parametričnim
   tipom /'a drevo/ z enim konstruktorjem, ki sprejme:
   - vrednost (koren) tipa /'a/ in
   - seznam (gozd) dreves tipa /'a drevo/. *)

type 'a drevo = Drevo of 'a * ('a drevo) list

(* 2.2) Definirajte naslednja rožna drevesa:

   t = 1,  t' =  2   ,      t'' =  3
                / \               /| \
               t  t              / |  \
                               -1  t'  0

 *)

let t = Drevo (1, [])
let t' = Drevo (2, [t;t])
let t'' = Drevo (3, [Drevo (-1, []); t'; Drevo(0, [])])

(* 2.3) Definirajte funkcijo, ki vrne gozd rožnega drevesa. *)

let vrni_gozd = function
    | Drevo ( _ , gozd) -> gozd

(* 2.4) Definirajte funkcijo, ki izpiše vse vrednosti v rožnem drevesu celih števil.
   Števila naj bodo v ločenih vrsticah. Uporabite (print_int : int -> unit) in
   (print_newline : unit -> unit). *)


let rec izpisi_vrednosti_sez sez f = 
        match sez with
        | [] -> ()
        | x :: xs -> 
            f x;
            izpisi_vrednosti_sez xs f


let rec izpisi_vrednosti tree = 
    match tree with
    | Drevo (n, []) ->
        print_int n;
        print_newline ()
    | Drevo (n, t :: ts) ->
        print_int n;
        print_newline ();
        izpisi_vrednosti_sez (t :: ts) izpisi_vrednosti


let rec izpisi_vrednosti' tree = 
    match tree with
    | Drevo (n, []) ->
        print_int n;
        print_newline ()
    | Drevo (n, t :: ts) ->
        print_int n;
        print_newline ();
        izpisi_vrednosti' t;
        let rec izpisi_vr_sez = function
            | [] -> ()
            | x :: xs -> 
                izpisi_vrednosti' x;
                izpisi_vr_sez xs
        in izpisi_vr_sez ts

(* 2.5) Definirajte funkcijo, ki izračuna globino rožnega drevesa, t.j. dolžino
   najdaljše poti od korena do lista. *)

let rec globina tree = 
    let rec globina_v_sez sez =
        match sez with
        | [] -> 0
        | t :: ts -> max (globina t) (globina_v_sez ts)
    in
    match tree with
    | Drevo ( _ , []) -> 1
    | Drevo ( _ , x :: xs) -> 1 + max (globina x) (globina_v_sez xs)

(* 2.6) Definirajte funkcijo, ki sestavi (poljubno) rožno drevo globine n.
   Vrednosti v korenih so poljubne. *)

let rec globoko_drevo n x = 
    match n with
    | 1 -> Drevo (x, [])
    | n -> Drevo (x, [globoko_drevo (n - 1) x])

(* 2.7) Definirajte funkcijo, ki sprejme funkcijo (f : 'b -> 'a -> 'b) in začetno vrednost (acc : 'b)
   in funkcijo f zloži [fold] preko drevesa (t : 'a drevo). Vrstni red pri tem ni pomemben.

   Za primer t' želimo vrednost f (f (f acc 1) 2) 2)  (lahko tudi f (f (f acc 2) 1) 2))
   Primer: /zlozi (fun acc x -> x + acc) 0 t'' = 6/

   Če želite vse točke, mora biti funkcija repno rekurzivna.

   Opomba: kot ste videli na vajah, nekatere funkcije iz modula List,
   na primer List.map, niso repno rekurzivne, zato se jim raje
   izognite. *)

let reverse xs =
  let rec aux xs acc = 
    match xs with
    | [] -> acc
    | x :: xs' -> aux xs' (x :: acc)
  in aux xs [] 

let stakni sez1 sez2 = 
    let rec aux sez1 acc =
        match reverse sez1 with
        | [] -> acc
        | x :: xs -> aux (reverse xs) (x :: acc)
    in aux sez1 sez2 

let rec drevo_v_sez tree = 
    let rec aux tree acc = 
    match tree with
    | Drevo (a, [] ) -> a :: acc 
    | Drevo (a, sez) -> 
        let rec po_sez sez acc =
            match sez with
            | [] -> acc
            | t :: ts -> po_sez ts (stakni (drevo_v_sez t) acc)
        in po_sez sez (a :: acc)
    in aux tree []

let rec fold_left_no_acc f sez = 
  match sez with
  | [] -> failwith "Prekratek seznam"
  | [_] -> failwith "Prekratek seznam"
  | [x; y] -> f x y
  | x :: y :: xs -> fold_left_no_acc f ((f x y) :: xs)

let zlozi f acc tree = fold_left_no_acc f (acc :: drevo_v_sez tree)  

