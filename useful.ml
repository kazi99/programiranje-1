(* Vse je repno rekuzivno *)
(* |||||||||||||||||||||||||||||||||||||||||||||||||||||||| *)

let reverse xs =
  let rec aux xs acc = 
    match xs with
    | [] -> acc
    | x :: xs' -> aux xs' (x :: acc)
  in aux xs [] 

(* |||||||||||||||||||||||||||||||||||||||||||||||||||||||| *)

let starting_element = function
    | [] -> failwith "Error"
    | x :: _ -> x

(* |||||||||||||||||||||||||||||||||||||||||||||||||||||||| *)

let rec loop condition f x =
  let rec loop' condition f x acc = 
    match condition x with
    | false -> acc
    | true -> loop' condition f (f x) (f x)
  in
  loop' condition f x x 

(* |||||||||||||||||||||||||||||||||||||||||||||||||||||||| *)

let rec fold_left_no_acc f sez = 
  match sez with
  | [] | [ _ ] -> failwith "Prekratek seznam"
  | [x; y] -> f x y
  | x :: y :: xs -> fold_left_no_acc f ((f x y) :: xs) 

(* |||||||||||||||||||||||||||||||||||||||||||||||||||||||| *)

let rec member_in_list mem = function
  | [] -> false
  | x :: xs' -> if x = mem then true else member_in_list mem xs'

(* |||||||||||||||||||||||||||||||||||||||||||||||||||||||| *)
(* rabi reverse *)
(* -------------------------------------------------------- *)

let append xs x = reverse (x :: (reverse xs))

(* -------------------------------------------------------- *)

let rec remove y xs = 
  let rec aux xs acc = 
    match xs with
    | [] -> acc
    | x :: xs' -> 
      if x = y then aux xs' acc else aux xs' (x :: acc)
  in reverse (aux xs [])

(* -------------------------------------------------------- *)

(* dela isto kot @ ampak je repno rekurzivna *)
let stakni sez1 sez2 = 
    let rec aux sez1 acc =
        match reverse sez1 with
        | [] -> acc
        | x :: xs -> aux (reverse xs) (x :: acc)
    in aux sez1 sez2 

(* -------------------------------------------------------- *)

let remove_last xs = 
  match reverse xs with
  | [] -> []
  | _ :: xs -> reverse xs

(* -------------------------------------------------------- *)

let rec map_tlrec f list =
  let rec map_tlrec' f list acc = 
    match list with
    | [] -> acc
    | x :: xs -> map_tlrec' f xs (f x :: acc)
  in
  reverse(map_tlrec' f list [])
  
(* -------------------------------------------------------- *)
(* |||||||||||||||||||||||||||||||||||||||||||||||||||||||| *)
(* rabi starting_element *)
(* -------------------------------------------------------- *)

let largest xs = 
  let rec largest' xs big = 
    match xs with
    | [] -> big
    | x :: xs' -> if x >= big then largest' xs' x else largest' xs' big
  in largest' xs (starting_element xs)

(* |||||||||||||||||||||||||||||||||||||||||||||||||||||||| *)

type euro = Euro of float  
(* Euro je konstruktor tipa euro *)

type wizard = {name: string; age: int} (* to je zapisni tip *)

type 'a drevo = Drevo of 'a * ('a drevo) list (* parametricni tip, parameter je 'a *)