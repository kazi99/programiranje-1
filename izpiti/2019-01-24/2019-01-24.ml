(* 1. naloga *)

(* a *)

let podvoji_vsoto a b = 2 * (a + b)

(* b *)

let povsod_vecji (a, b, c) (d, e, f) = a > d && b > e && c > f

(* c *)

let uporabi_ce_lahko f = function
  | None -> None
  | Some x -> Some (f x)

(* d *)

let rec pojavi_dvakrat a xs = 
  let rec aux a xs st = match xs with
  | [] -> st
  | x :: xs' when x = a -> aux a xs' (st + 1)
  | x :: xs' -> aux a xs' st
  in aux a xs 0 = 2

(* e *)

let izracunaj_v_tocki a fs =
  let rec aux a fs acc = match fs with
  | [] -> acc
  | f :: fs' -> aux a fs' ((f a) :: acc)
  in List.rev (aux a fs [])

let f = fun x -> x + 1
let g = fun x -> x + 2
let h = fun x -> x + 4 

(* d *)

let eksponent x p =
  let rec aux x p acc = match p with
  | 0 -> acc
  | p -> aux x (p - 1) (acc * x)
  in aux x p 1

(* 2. naloga *)

(* a *)

type 'a mm_drevo =
  | Empty
  | Node of 'a mm_drevo * 'a * int * 'a mm_drevo

let t = 
Node(
  Node(
    Empty, 1, 3, Empty
  ),
  2, 1,
  Node(
    Node(
      Empty, 4, 1, Empty
    ),
    5, 1,
    Node(
      Empty, 8, 2, Empty
    )
  )
)

(* b *)

let rec vstavi a = function
  | Empty -> Node (Empty, a, 1, Empty)
  | Node (l, x, n, d) when a < x -> Node (vstavi a l, x, n, d)
  | Node (l, x, n, d) when a > x -> Node (l, x, n, vstavi a d)
  | Node (l, x, n, d) -> Node (l, x, n + 1, d) 

(* c *)

let rec multimnozica_iz_seznama = function
  | [] -> Empty
  | x :: xs -> vstavi x (multimnozica_iz_seznama xs)

(* d *)

let rec velikost_multimnozice mm = match mm with
  | Empty -> 0
  | Node (l, _, n, d) -> n + (velikost_multimnozice l) + (velikost_multimnozice d)

(* e *)

let multiply x n = 
  let rec aux n acc = match n with
  | 0 -> acc
  | n -> aux (n - 1) (x :: acc)
  in aux n []

let rec seznam_iz_multimnozice_not_tr mm = match mm with
  | Empty -> []
  | Node (l, x, n, d) -> seznam_iz_multimnozice_not_tr l @ (multiply x n) @ seznam_iz_multimnozice_not_tr d 

(* ta vrjetno tudi ni tr *)
let seznam_iz_multimnozice mm = 
  let rec aux mm acc = match mm with
  | Empty -> acc
  | Node (l, x, n, d) -> aux l (aux d (List.rev_append (multiply x n) acc))
  in aux mm [] 

let test = seznam_iz_multimnozice t

let rec large_mm = function
  | 0 -> Empty
  | n -> vstavi n (large_mm (n - 1))