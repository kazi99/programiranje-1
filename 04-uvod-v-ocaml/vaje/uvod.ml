
(* ========== Vaja 1: Uvod v OCaml  ========== *)

let reverse xs =
  let rec aux xs acc = 
    match xs with
    | [] -> acc
    | x :: xs' -> aux xs' (x :: acc)
  in aux xs [] 

let append xs x = reverse (x :: (reverse xs))

(*----------------------------------------------------------------------------*]
 Funkcija [square] vrne kvadrat podanega celega števila.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # square 2;;
 - : int = 4
[*----------------------------------------------------------------------------*)

let square x = x * x

(*----------------------------------------------------------------------------*]
 Funkcija [middle_of_triple] vrne srednji element trojice.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # middle_of_triple (true, false, true);;
 - : bool = false
[*----------------------------------------------------------------------------*)

let middle_of_triple (_,b,_) = b

(* drug nacin*)

let middle x = 
  let (_,b,_) = x
  in b

(*----------------------------------------------------------------------------*]
 Funkcija [starting_element] vrne prvi element danega seznama. V primeru
 prekratkega seznama vrne napako.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # starting_element [1; 2; 3; 4];;
 - : int = 1
[*----------------------------------------------------------------------------*)

let starting_element = function
  | [] -> failwith "ERROR"
  | x :: _ -> x


let starting_element' sez = 
  match sez with
  | [] -> failwith "ERROR"
  | x :: _ -> x

let starting_element'' sez = 
  match sez with
  | x :: _ -> x             (* tole ze ugotovi da bo argument seznam *) 
  | _ -> failwith "ERROR"   (* za vse ostale vzorce*)


(*----------------------------------------------------------------------------*]
 Funkcija [multiply] zmnoži vse elemente seznama. V primeru praznega seznama
 vrne vrednost, ki je smiselna za rekurzijo.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # multiply [2; 4; 6];;
 - : int = 48
[*----------------------------------------------------------------------------*)

let rec multiply sez = 
  match sez with
  | x :: xs -> x * multiply xs 
  | _ -> 1

let rec multiply' = function
  | [] -> 1
  | x :: xs -> x * multiply' xs

(*----------------------------------------------------------------------------*]
 Napišite funkcijo ekvivalentno python kodi:

  def sum_int_pairs(pair_list):
      if len(pair_list) == 0:
        return []
      else:
        x, y = pair_list[0]
        return [x + y] + sum_int_pairs(pair_list[1:])

 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # sum_int_pairs [(1, -2); (3, 4); (0, -0)];;
 - : int list = [-1; 7; 0]
[*----------------------------------------------------------------------------*)

let rec sum_int_pairs sez =
  match sez with
  | [] -> []
  | (a,b) :: xs -> a + b :: sum_int_pairs xs

(*----------------------------------------------------------------------------*]
 Funkcija [get k list] poišče [k]-ti element v seznamu [list]. Številčenje
 elementov seznama (kot ponavadi) pričnemo z 0. Če je k negativen, funkcija
 vrne ničti element. V primeru prekratkega seznama funkcija vrne napako.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # get 2 [0; 0; 1; 0; 0; 0];;
 - : int = 1
[*----------------------------------------------------------------------------*)

let rec get indeks sez =
  match sez with
  | [] -> failwith "Prevelik indeks"
  | x :: xs -> if indeks <= 0 then x else get (indeks - 1) xs


let rec get' indeks = function
  | [] -> failwith "Prevelik indeks"
  | x :: _ when indeks <= 0 -> x
  | x :: xs -> get (indeks - 1) xs

(*----------------------------------------------------------------------------*]
 Funkcija [double] podvoji pojavitve elementov v seznamu.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # double [1; 2; 3];;
 - : int list = [1; 1; 2; 2; 3; 3]
[*----------------------------------------------------------------------------*)

let rec double = function
  | [] -> []
  | x :: xs -> x :: (x :: double xs)
(*  | x :: xs -> [x;x] :: double xs *)

(*----------------------------------------------------------------------------*]
 Funkcija [insert x k list] na [k]-to mesto seznama [list] vrine element [x].
 Če je [k] izven mej seznama, ga funkcija doda na začetek oziroma na konec.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # insert 1 3 [0; 0; 0; 0; 0];;
 - : int list = [0; 0; 0; 1; 0; 0]
 # insert 1 (-2) [0; 0; 0; 0; 0];;
 - : int list = [1; 0; 0; 0; 0; 0]
[*----------------------------------------------------------------------------*)

let rec insert el k xs =
  if k <= 0 then el :: xs else 
  let rec aux el k xs acc = 
    match xs with
      | [] -> append acc el
      | x :: xs' -> 
        if k = 0 then (append acc x) @ (el :: xs')
        else aux el (k - 1) xs' (append acc x)
  in aux el (k - 1) xs []

(*----------------------------------------------------------------------------*]
 Funkcija [divide k list] seznam razdeli na dva seznama. Prvi vsebuje prvih [k]
 elementov, drugi pa vse ostale. Funkcija vrne par teh seznamov. V primeru, ko
 je [k] izven mej seznama, je primeren od seznamov prazen.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # divide 2 [1; 2; 3; 4; 5];;
 - : int list * int list = ([1; 2], [3; 4; 5])
 # divide 7 [1; 2; 3; 4; 5];;
 - : int list * int list = ([1; 2; 3; 4; 5], [])
[*----------------------------------------------------------------------------*)

let rec divide k sez = 
  if k <= 0 then
    ([], sez)
  else
    match sez with
    | x :: xs -> 
      let prej, potem = divide (k - 1) xs in
      (x :: prej, potem)
    | [] -> ([], [])

(*----------------------------------------------------------------------------*]
 Funkcija [rotate n list] seznam zavrti za [n] mest v levo. Predpostavimo, da
 je [n] v mejah seznama.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # rotate 2 [1; 2; 3; 4; 5];;
 - : int list = [3; 4; 5; 1; 2]
[*----------------------------------------------------------------------------*)

let rec rotate k xs = 
  if k <= 0 then xs else
    match xs with
    | [] -> []
    | x :: xs' -> rotate (k - 1) (xs' @ [x])

let rec rotate_tl k xs = 
  if k <= 0 then xs else
    match xs with
    | [] -> []
    | x :: xs' -> rotate_tl (k - 1) (append xs' x)

(*----------------------------------------------------------------------------*]
 Funkcija [remove x list] iz seznama izbriše vse pojavitve elementa [x].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # remove 1 [1; 1; 2; 3; 1; 2; 3; 1; 1];;
 - : int list = [2; 3; 2; 3]
[*----------------------------------------------------------------------------*)

let rec remove y xs = 
  let rec aux xs acc = 
    match xs with
    | [] -> acc
    | x :: xs' -> 
      if x = y then aux xs' acc else aux xs' (x :: acc)
  in reverse (aux xs [])

(*----------------------------------------------------------------------------*]
 Funkcija [is_palindrome] za dani seznam ugotovi ali predstavlja palindrom.
 Namig: Pomagaj si s pomožno funkcijo, ki obrne vrstni red elementov seznama.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # is_palindrome [1; 2; 3; 2; 1];;
 - : bool = true
 # is_palindrome [0; 0; 1; 0];;
 - : bool = false
[*----------------------------------------------------------------------------*)

let remove_last xs = 
  match reverse xs with
  | [] -> []
  | _ :: xs -> reverse xs

let rec is_palindrome xs =
  match xs with 
  | [] -> true
  | [a] -> true
  | x :: xs' -> 
    if x = starting_element (reverse xs') then is_palindrome (remove_last xs') else false 

(*----------------------------------------------------------------------------*]
 Funkcija [max_on_components] sprejme dva seznama in vrne nov seznam, katerega
 elementi so večji od istoležnih elementov na danih seznamih. Skupni seznam ima
 dolžino krajšega od danih seznamov.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # max_on_components [5; 4; 3; 2; 1] [0; 1; 2; 3; 4; 5; 6];;
 - : int list = [5; 4; 3; 3; 4]
[*----------------------------------------------------------------------------*)

let rec max_on_components xs ys = 
  let rec aux xs ys acc = 
    match xs, ys with
    | [], [] -> acc
    | x :: _, [] -> acc
    | [], y :: _ -> acc
    | x :: xs', y :: ys' -> 
      if x >= y then aux xs' ys' (x :: acc) else aux xs' ys' (y :: acc)
  in reverse (aux xs ys [])

(*----------------------------------------------------------------------------*]
 Funkcija [second_largest] vrne drugo največjo vrednost v seznamu. Pri tem se
 ponovitve elementa štejejo kot ena vrednost. Predpostavimo, da ima seznam vsaj
 dve različni vrednosti.
 Namig: Pomagaj si s pomožno funkcijo, ki poišče največjo vrednost v seznamu.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # second_largest [1; 10; 11; 11; 5; 4; 10];;
 - : int = 10
[*----------------------------------------------------------------------------*)

let largest xs = 
  let rec largest' xs big = 
    match xs with
    | [] -> big
    | x :: xs' -> if x >= big then largest' xs' x else largest' xs' big
  in largest' xs (starting_element xs)

let second_largest xs = largest (remove (largest xs) xs)
