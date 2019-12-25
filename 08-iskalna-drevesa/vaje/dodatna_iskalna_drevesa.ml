(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 DODATNE VAJE 
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

type 'a tree =
  | Empty
  | Node of 'a tree * 'a * 'a tree

let make_leaf x = Node(Empty, x, Empty)

let rec insert v = function
  | Empty -> make_leaf v
  | Node(l, x, d) when v < x -> Node(insert v l, x, d)
  | Node(l, x, d) when v > x -> Node(l, x, insert v d)
  | t -> t

let is_between lb x up = 
  match lb, up with
  | (None, None) -> true
  | (None, Some up') -> x < up'
  | (Some lb', None) -> lb' < x
  | (Some lb', Some up') -> (lb' < x) && (x < up') 


let is_bst =
  let rec is_bst_aux cur_min cur_max = function
    | Empty -> true
    | Node(l, x, d) -> (is_between cur_min x cur_max)
                    && (is_bst_aux cur_min (Some x) l)
                    && (is_bst_aux (Some x) cur_max d)
  in
  is_bst_aux None None

(*----------------------------------------------------------------------------*]
 Funkcija [bst_of_list] iz seznama naredi dvojiško iskalno drevo.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # [11; 6; 7; 0; 2; 5] |> bst_of_list |> is_bst;;
 - : bool = true
[*----------------------------------------------------------------------------*)

let bst_of_list = 
    let rec aux acc = function
    | [] -> acc
    | x :: xs -> aux (insert x acc) xs
    in
    aux Empty

(*----------------------------------------------------------------------------*]
 Funkcija [tree_sort] uredi seznam s pomočjo pretvorbe v bst in nato nazaj
 v seznam.

 Opomba: Prosim ne uporabljajte te funkcije v praksi.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # tree_sort ["a"; "c"; "f"; "b"; "e"; "d"];;
 - : string list = ["a"; "b"; "c"; "d"; "e"; "f"]
[*----------------------------------------------------------------------------*)

let rec list_of_tree = function
  | Empty -> []
  | Node(l, x, d) -> (list_of_tree l) @ (x :: (list_of_tree d))

let tree_sort l = list_of_tree (bst_of_list l) 

(*----------------------------------------------------------------------------*]
 Funkcija [follow directions tree] tipa [direction list -> 'a tree -> 'a option]
 sprejme seznam navodil za premikanje po drevesu in vrne vozlišče do katerega 
 vodi podana pot. Ker navodila morda ne vodijo do nobenega vozlišča v drevesu
 vrne rezultat kot [option] tip. Ne pozabite definirati tipa [directions].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # follow [Right; Left] test_tree;;
 - : int option = Some 6
 # follow [Right; Left; Right; Right] test_tree;;
 - : int option = None
[*----------------------------------------------------------------------------*)

let test_tree = Node(Node(make_leaf 0, 2, Empty) , 5, Node(make_leaf 6, 7, make_leaf 11))

(*        5
         / \
        2   7
       /   / \
      0   6   11       *)

type directions = Left | Right

let follow directions tree = 
  let rec follow' (directions: directions list) (tree: 'a tree option) = 
    match directions with
    | [] -> (match tree with
      | None | Some Empty -> None
      | Some Node(_, x, _) -> Some x
    )
    | Left :: xs -> (match tree with
      | None | Some Empty -> None
      | Some Node(l, _, _) -> follow' xs (Some l)
    )
    | Right :: xs -> (match tree with
      | None | Some Empty -> None
      | Some Node(_, _, r) -> follow' xs (Some r)
    )
  in
  follow' directions (Some tree)

(*----------------------------------------------------------------------------*]
 Funkcija [prune directions tree] poišče vozlišče v drevesu glede na navodila,
 ter izbriše poddrevo, ki se začne v izbranem vozlišču.

 Opozorilo: Pri uporabi [Some Node(l, x, r)] se OCaml pritoži, saj to razume 
 kot [(Some Node)(l, x, r)], zato pravilno postavite potrebne oklepaje.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # prune [Right] test_tree;;
 - : int tree option =
 Some (Node (Node (Node (Empty, 0, Empty), 2, Empty), 5, Empty))
[*----------------------------------------------------------------------------*)

let rec delete_sub_tree x tree = 
  match tree with
  | Empty -> tree
  | Node(l, y, d) when x < y -> Node(delete_sub_tree x l, y, d) 
  | Node(l, y, d) when x > y -> Node(l, y, delete_sub_tree x d) 
  | _ -> Empty (* to je se edina preostala moznost ko sta x=y in takrat poddrevo zbrisemo *)

let prune directions tree = 
  let to_clear = follow directions tree in
  match to_clear with
  | None -> tree
  | Some x -> delete_sub_tree x tree

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 PHANTOM TREES

 Druga možnost pri brisanju podatkov je, da spremenimo tip s katerim
 predstavljamo drevo. Definirate nov tip fantomskega drevesa, ki poleg podatka,
 levega in desnega poddrevesa hrani še dodatno informacijo o stanju [state], ki
 je bodisi [Exists] če je vozlišče še prisotno in pa [Ghost] če je vozlišče v
 drevesu izbrisano in ga upoštevamo le še kot delitveno vozlišče. Še vedno
 predpostavljamo, da imajo drevesa obliko BST.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

type state = Exists | Ghost

type 'a phantom_tree = 
  | P_Empty
  | P_Node of 'a phantom_tree * 'a  * 'a phantom_tree * state

(*----------------------------------------------------------------------------*]
 Funkcija [phantomize] tipa ['a tree -> 'a phantom_tree] navadnemu drevesu
 priredi ekvivalentno fantomsko drevo.
 Funkcija [kill x ptree] izbriše element [x] v fantomskem drevesu tako, da 
 njegovo stanje nastavi na [Ghost].
 Predpostavite lahko, da v drevesu ni ponovitev elementov.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # phantomize test_tree;;
 - : int phantom_tree =
 P_Node (P_Node (P_Node (P_Empty, 0, P_Empty, Exists), 2, P_Empty, Exists), 5,
 P_Node (P_Node (P_Empty, 6, P_Empty, Exists), 7,
 P_Node (P_Empty, 11, P_Empty, Exists), Exists),
 Exists)

 # bst_of_list [3; 4; 2] |> phantomize |> kill 3 |> kill 6;;
 - : int phantom_tree =
 P_Node (P_Empty, 2,
 P_Node (P_Node (P_Empty, 3, P_Empty, Ghost), 4, P_Empty, Exists), Exists)

Zgleda da bst_of_list dela mal drugace pri meni kot pri njih

P_Node (
    P_Empty, 
    2,
    P_Node (
        P_Node (P_Empty, 3, P_Empty, Ghost), 
        4, 
        P_Empty, Exists), Exists)

P_Node (
    P_Node (P_Empty, 2, P_Empty, Exists), 
    3, 
    P_Node (P_Empty, 4, P_Empty, Exists), Ghost)

[*----------------------------------------------------------------------------*)

let rec phantomize = function
  | Empty -> P_Empty
  | Node (d, x, r) -> P_Node (phantomize d, x, phantomize r, Exists) 

let rec kill x ptree =
  match ptree with
  | P_Empty -> P_Empty
  | P_Node (l, y, r, _) when y = x -> P_Node (l, y, r, Ghost)
  | P_Node (l, y, r, s) when x > y -> P_Node (l, y, kill x r, s)
  | P_Node (l, y, r, s) -> P_Node (kill x l, y, r, s)

(*----------------------------------------------------------------------------*]
 Funkcija [unphantomize] tipa ['a phantom_tree -> 'a tree] fantomskemu drevesu 
 priredi navadno drevo, ki vsebuje zgolj vozlišča, ki še obstajajo. Vrstni red
 vozlišč v končnem drevesu ni pomemben.

 Namig: Lahko uporabite vmesni prehodom na drugo podatkovno strukturo.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # test_tree |> phantomize |> kill 7 |> kill 0 |> kill 5 |> unphantomize;;
 - : int tree = Node (Node (Node (Empty, 2, Empty), 6, Empty), 11, Empty)
[*----------------------------------------------------------------------------*)

let rec list_of_ptree = function
  | P_Empty -> []
  | P_Node (l, x, r, s) when s = Exists -> (list_of_ptree l) @ (x :: list_of_ptree r)
  | P_Node (l, x, r, s) -> (list_of_ptree l) @ (list_of_ptree r)

let unphantomize ptree = ptree |> list_of_ptree |> bst_of_list