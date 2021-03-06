(* ========== Vaja 4: Iskalna Drevesa  ========== *)

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 Ocaml omogoča enostavno delo z drevesi. Konstruiramo nov tip dreves, ki so
 bodisi prazna, bodisi pa vsebujejo podatek in imajo dve (morda prazni)
 poddrevesi. Na tej točki ne predpostavljamo ničesar drugega o obliki dreves.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

type 'a tree =
  | Empty
  | Node of 'a tree * 'a * 'a tree

(*----------------------------------------------------------------------------*]
 Definirajmo si testni primer za preizkušanje funkcij v nadaljevanju. Testni
 primer predstavlja spodaj narisano drevo, pomagamo pa si s pomožno funkcijo
 [leaf], ki iz podatka zgradi list.
          5
         / \
        2   7
       /   / \
      0   6   11
[*----------------------------------------------------------------------------*)

let make_leaf x = Node(Empty, x, Empty)

let test_tree = Node(Node(make_leaf 0, 2, Empty) , 5, Node(make_leaf 6, 7, make_leaf 11))
(*----------------------------------------------------------------------------*]
 Funkcija [mirror] vrne prezrcaljeno drevo. Na primeru [test_tree] torej vrne
          5
         / \
        7   2
       / \   \
      11  6   0
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # mirror test_tree ;;
 - : int tree =
 Node (Node (Node (Empty, 11, Empty), 7, Node (Empty, 6, Empty)), 5,
 Node (Empty, 2, Node (Empty, 0, Empty)))
[*----------------------------------------------------------------------------*)

let rec mirror tree = 
  match tree with
  | Empty -> Empty
  | Node(l, x, d) -> Node(mirror d, x, mirror l)

(* ce mirror dvakrat uporabimo ni tail recursive *)

(*----------------------------------------------------------------------------*]
 Funkcija [height] vrne višino oz. globino drevesa, funkcija [size] pa število
 vseh vozlišč drevesa.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # height test_tree;;
 - : int = 3
 # size test_tree;;
 - : int = 6
[*----------------------------------------------------------------------------*)

let rec height = function
  | Empty -> 0
  | Node(l, _ , d) -> 1 + max (height l) (height d)

let rec size = function
  | Empty -> 0
  | Node(l, _ , d) -> 1 + (size l) + (size d)

(*----------------------------------------------------------------------------*]
 Funkcija [map_tree f tree] preslika drevo v novo drevo, ki vsebuje podatke
 drevesa [tree] preslikane s funkcijo [f].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # map_tree ((<)3) test_tree;;
 - : bool tree =
 Node (Node (Node (Empty, false, Empty), false, Empty), true,
 Node (Node (Empty, true, Empty), true, Node (Empty, true, Empty)))
[*----------------------------------------------------------------------------*)

let rec map_tree f tree = 
  match tree with
  | Empty -> Empty
  | Node(l, x, d) -> Node(map_tree f l, f x, map_tree f d)

(*----------------------------------------------------------------------------*]
 Funkcija [list_of_tree] pretvori drevo v seznam. Vrstni red podatkov v seznamu
 naj bo takšen, da v primeru binarnega iskalnega drevesa vrne urejen seznam.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # list_of_tree test_tree;;
 - : int list = [0; 2; 5; 6; 7; 11]
[*----------------------------------------------------------------------------*)

let rec list_of_tree = function
  | Empty -> []
  | Node(l, x, d) -> (list_of_tree l) @ (x :: (list_of_tree d))

(* Časovna zahtevnost je O(n) *)

(* 
  T(n) = T(n/2) + T_@(n/2, n/2) + O(1) + T(n/2) 
  ( Vemo T_@(n/2, n/2) = O(n/2) = O(n))
       = T(n/2) + O(n/2) + O(1) + T(n/2)
       = 2T(n/2) + O(n/2) + O(1)
       ...
       = 2^(Log(n))O(1) + ???

*)

(*----------------------------------------------------------------------------*]
 Funkcija [is_bst] preveri ali je drevo binarno iskalno drevo (Binary Search 
 Tree, na kratko BST). Predpostavite, da v drevesu ni ponovitev elementov, 
 torej drevo npr. ni oblike Node( leaf 1, 1, leaf 2)). Prazno drevo je BST.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # is_bst test_tree;;
 - : bool = true
 # test_tree |> mirror |> is_bst;;
 - : bool = false
[*----------------------------------------------------------------------------*)

let rec is_sorted = function
  | [] | [_] -> true
  | x :: y :: xs -> x < y && is_sorted (y :: xs) (* tale je uresnici tail recursive, ker najprej preveri x<y *)

let rec is_sorted' = function
  | x :: y :: xs -> if x < y then is_sorted'(y :: xs) else false
  | _ -> true

let is_bst tree = is_sorted (list_of_tree tree)



let is_between lb x up = 
  match lb, up with
  | (None, None) -> true
  | (None, Some up') -> x < up'
  | (Some lb', None) -> lb' < x
  | (Some lb', Some up') -> (lb' < x) && (x < up') 


let is_bst_eff =
  let rec is_bst_aux cur_min cur_max = function
    | Empty -> true
    | Node(l, x, d) -> (is_between cur_min x cur_max)
                    && (is_bst_aux cur_min (Some x) l)
                    && (is_bst_aux (Some x) cur_max d)
  in
  is_bst_aux None None

(* let is_bst_eff =
  let rec is_bst_aux cur_min cur_max = function
    | Empty -> true
    | Node(l, x, d) -> cur_min < x && x < cur_max && (is_bst_aux cur_min x l) && (is_bst_aux x cur_max d)
  in
  is_bst_aux None None *)
  


(* let je_vmes tree x y = *)

(* let is_bst tree = function
  | Empty | Node(Empty, x, Empty) -> true
  | Node(Empty, x, d) -> 
  | Node(l, x, Empty) -> 
  | Node(l, x, d) -> is_bst l && is_bst d &&  *)

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 V nadaljevanju predpostavljamo, da imajo dvojiška drevesa strukturo BST.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

(*----------------------------------------------------------------------------*]
 Funkcija [insert] v iskalno drevo pravilno vstavi dani element. Funkcija 
 [member] preveri ali je dani element v iskalnem drevesu.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # insert 2 (leaf 4);;
 - : int tree = Node (Node (Empty, 2, Empty), 4, Empty)
 # member 3 test_tree;;
 - : bool = false
[*----------------------------------------------------------------------------*)

let rec member v = function
  | Empty -> false
  | Node(l, x, d) -> 
    if v < x then member v l else  
    if x < v then member v d else true


let rec member' v = function
  | Empty -> false
  | Node(l, x, d) -> 
    if x < v && x > v then true else
    if v < x then member' v d else member' v l  

let rec insert v = function
  | Empty -> make_leaf v
  | Node(l, x, d) when v < x -> Node(insert v l, x, d)
  | Node(l, x, d) when v > x -> Node(l, x, insert v d)
  | t -> t

(*----------------------------------------------------------------------------*]
 Funkcija [member2] ne privzame, da je drevo bst.
 
 Opomba: Premislte kolikšna je časovna zahtevnost funkcije [member] in kolikšna
 funkcije [member2] na drevesu z n vozlišči, ki ima globino log(n). 
[*----------------------------------------------------------------------------*)

let rec member2 mem = function
  | Empty -> false
  | Node(l, x, d) -> x = mem || member2 mem l || member2 mem d

let rec member_in_list mem = function
  | [] -> false
  | x :: xs' -> if x = mem then true else member_in_list mem xs'

let member3 mem bst = member_in_list mem (list_of_tree bst)  

(*----------------------------------------------------------------------------*]
 Funkcija [succ] vrne naslednika korena danega drevesa, če obstaja. Za drevo
 oblike [bst = Node(l, x, r)] vrne najmanjši element drevesa [bst], ki je večji
 od korena [x].
 Funkcija [pred] simetrično vrne največji element drevesa, ki je manjši od
 korena, če obstaja.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # succ test_tree;;
 - : int option = Some 6
 # pred (Node(Empty, 5, leaf 7));;
 - : int option = None
[*----------------------------------------------------------------------------*)

let succ bst = 
  let rec minimal = function
    | Empty -> None
    | Node(Empty, x, _) -> Some x
    | Node(l, _, _) -> minimal l
  in
  match bst with
  | Empty -> None
  | Node(l, x, d) -> minimal d

let pred bst =
  let rec maximal = function
    | Empty -> None
    | Node(_, x, Empty) -> Some x
    | Node(_, _, d) -> maximal d
  in
  match bst with
  | Empty -> None
  | Node(l, _, _) -> maximal l

(*----------------------------------------------------------------------------*]
 Na predavanjih ste omenili dva načina brisanja elementov iz drevesa. Prvi 
 uporablja [succ], drugi pa [pred]. Funkcija [delete x bst] iz drevesa [bst] 
 izbriše element [x], če ta v drevesu obstaja. Za vajo lahko implementirate
 oba načina brisanja elementov.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # (*<< Za [delete] definiran s funkcijo [succ]. >>*)
 # delete 7 test_tree;;
 - : int tree =
 Node (Node (Node (Empty, 0, Empty), 2, Empty), 5,
 Node (Node (Empty, 6, Empty), 11, Empty))
[*----------------------------------------------------------------------------*)

(* ta funkcija brise samo liste na koncu, sicer ne dela *)
let rec delete_leaf x bst = 
  match x with
  | None -> Empty
  | Some x -> (
    match bst with
    | Node(l, y, d) when x < y -> Node(delete_leaf (Some x) l, y, d)
    | Node(l, y, d) when x > y -> Node(l, y, delete_leaf (Some x) d)
    | Node(l, y, d) -> Empty
    | _ -> failwith "not happening" 
  )

let rec delete x bst = 
  if not (member x bst) then bst else
  match bst with
  | Node(l, y, d) when x < y -> Node(delete x l, y, d) 
  | Node(l, y, d) when x > y -> Node(l, y, delete x d) 
  | Node(l, y, d) when x = y -> (
    let dummy_tree = Node(l, y, d) in
    match succ dummy_tree with
    | None -> Empty
    | Some s -> Node(l, s, delete_leaf (Some s) d)
  )
  | _ -> failwith ""

let rec delete_w_succ x bst = 
  if not (member x bst) then bst else
  match bst with
  | Node(l, y, d) when x < y -> Node(delete_w_succ x l, y, d) 
  | Node(l, y, d) when x > y -> Node(l, y, delete_w_succ x d) 
  | Node(l, y, d) as dummy_tree when x = y -> (
    match succ dummy_tree with
    | None -> Empty
    | Some s -> Node(l, s, delete_leaf (Some s) d)
  )
  | _ -> failwith ""

let rec delete_w_pred x bst = 
  if not (member x bst) then bst else
  match bst with
  | Node(l, y, d) when x < y -> Node(delete x l, y, d) 
  | Node(l, y, d) when x > y -> Node(l, y, delete x d) 
  | Node(l, y, d) as dummy_tree when x = y -> (
    match pred dummy_tree with
    | None -> Empty
    | Some p -> Node(delete_leaf (Some p) l, y, d)
  )
  | _ -> failwith ""

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 SLOVARJI

 S pomočjo BST lahko (zadovoljivo) učinkovito definiramo slovarje. V praksi se
 slovarje definira s pomočjo hash tabel, ki so še učinkovitejše. V nadaljevanju
 pa predpostavimo, da so naši slovarji [dict] binarna iskalna drevesa, ki v
 vsakem vozlišču hranijo tako ključ kot tudi pripadajočo vrednost, in imajo BST
 strukturo glede na ključe. Ker slovar potrebuje parameter za tip ključa in tip
 vrednosti, ga parametriziramo kot [('key, 'value) dict].
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

type ('key, 'value) dict = ('key * 'value) tree

(*----------------------------------------------------------------------------*]
 Napišite testni primer [test_dict]:
      "b":1
      /    \
  "a":0  "d":2
         /
     "c":-2
[*----------------------------------------------------------------------------*)

let test_dict : (string, int) dict = 
  Node (make_leaf ("a", 0), ("b", 1), Node (make_leaf ("c", -2), ("d", 2), Empty))

(*----------------------------------------------------------------------------*]
 Funkcija [dict_get key dict] v slovarju poišče vrednost z ključem [key]. Ker
 slovar vrednosti morda ne vsebuje, vrne [option] tip.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # dict_get "banana" test_dict;;
 - : 'a option = None
 # dict_get "c" test_dict;;
 - : int option = Some (-2)
[*----------------------------------------------------------------------------*)

let rec dict_get key d =
  match d with
  | Empty -> None
  | Node (l, (k, v), d) -> 
    if k = key then Some v else
    if k > key then dict_get key l
    else dict_get key d

(*----------------------------------------------------------------------------*]
 Funkcija [print_dict] sprejme slovar s ključi tipa [string] in vrednostmi tipa
 [int] in v pravilnem vrstnem redu izpiše vrstice "ključ : vrednost" za vsa
 vozlišča slovarja.
 Namig: Uporabite funkciji [print_string] in [print_int]. Nize združujemo z
 operatorjem [^]. V tipu funkcije si oglejte, kako uporaba teh funkcij določi
 parametra za tip ključev in vrednosti v primerjavi s tipom [dict_get].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # print_dict test_dict;;
 a : 0
 b : 1
 c : -2
 d : 2
 - : unit = ()
[*----------------------------------------------------------------------------*)

(* zelo podobno kot funkcija list_of_tree *)
let rec print_dict = function
  | Empty -> ()
  | Node (l, (key, value), d) ->
    print_dict l;
    print_string (key ^ " : ");
    print_int value;
    print_newline ();
    print_dict d

(*----------------------------------------------------------------------------*]
 Funkcija [dict_insert key value dict] v slovar [dict] pod ključ [key] vstavi
 vrednost [value]. Če za nek ključ vrednost že obstaja, jo zamenja.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # dict_insert "1" 14 test_dict |> print_dict;;
 1 : 14
 a : 0
 b : 1
 c : -2
 d : 2
 - : unit = ()
 # dict_insert "c" 14 test_dict |> print_dict;;
 a : 0
 b : 1
 c : 14
 d : 2
 - : unit = ()
[*----------------------------------------------------------------------------*)

let show_key (a, b) = a
let show_key' = fun (a, b) -> a

let rec dict_insert key value dict =
  let tree_of_keys = map_tree show_key dict in   (* ti dve vrstici sta useless*)
  if not (member key tree_of_keys) then insert (key, value) dict else 
  match dict with
  | Empty -> make_leaf (key, value)
  | Node(l, (k, v), d) when key > k -> Node (l, (k, v), dict_insert key value d)  
  | Node(l, (k, v), d) when key < k -> Node (dict_insert key value l, (k, v), d)
  | Node(l, (k, v), d) -> Node (l, (k, value), d)    (* to se izvede samo ko je k = key *)
  

let rec dict_insert' key value dict =
  match dict with
  | Empty -> make_leaf (key, value)
  | Node(l, (k, v), d) when key > k -> Node (l, (k, v), dict_insert' key value d)  
  | Node(l, (k, v), d) when key < k -> Node (dict_insert' key value l, (k, v), d)
  | Node(l, (k, v), d) -> Node (l, (k, value), d) 
