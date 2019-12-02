(* -------- 1 -------- *)

let vsota sez = 
    let rec aux sez acc = 
        match sez with
        | [] -> acc
        | x :: xs -> aux xs (x + acc)
    in aux sez 0

(* -------- 2 -------- *)

let je_urejen sez = 
    let rec aux sez vr = 
        match sez with
        | [] -> vr
        | [_] -> vr
        | a :: b :: xs -> 
            if a <= b then aux (b :: xs) (true && vr)
            else false 
    in aux sez true


let rec is_sorted = function
    | [] | [_] -> true
    | x :: y :: xs -> 
        if x <= y then is_sorted (y :: xs) else false

(* -------- 3 -------- *)

let ustavi x sez = 
    let rec aux x acc sez = 
        match sez with
        | [] -> acc @ [x]
        | a :: xs -> 
            if je_urejen (x :: a :: xs) then acc @ (x :: a :: xs)
            else aux x (acc @ [a]) xs
    in aux x [] sez

let uredi sez = 
    let rec aux acc sez = 
        match sez with
        | [] -> acc
        | a :: xs -> aux (ustavi a acc) xs
    in aux [] sez

(* -------- 4 -------- *)

let je_polj_urejen sez cmp = 
    let rec aux sez vr = 
        match sez with
        | [] -> vr
        | [_] -> vr
        | a :: b :: xs -> 
            if cmp a b then aux (b :: xs) (true && vr)
            else aux (b :: xs) false 
    in aux sez true

let polj_ustavi x sez cmp = 
    let rec aux x acc sez = 
        match sez with
        | [] -> acc @ [x]
        | a :: xs -> 
            if je_polj_urejen (x :: a :: xs) cmp then acc @ (x :: a :: xs)
            else aux x (acc @ [a]) xs
    in aux x [] sez

let polj_uredi sez cmp = 
    let rec aux acc sez = 
        match sez with
        | [] -> acc
        | a :: xs -> aux (polj_ustavi a acc cmp) xs
    in aux [] sez

(* polj_uredi [0;1;1;42] (fun j k -> not (j > k));; *)
(* -------- 5 -------- *)

type priority = 
    | Top
    | Group of int

type status = 
    | Staff
    | Passenger of priority

type flyer = { status : status ; name : string }

let flyers = [ {status = Staff; name = "Quinn"}
             ; {status = Passenger (Group 0); name = "Xiao"}
             ; {status = Passenger Top; name = "Jaina"}
             ; {status = Passenger (Group 1000); name = "Aleks"}
             ; {status = Passenger (Group 1000); name = "Robin"}
             ; {status = Staff; name = "Alan"}
             ]

(* -------- 6 -------- *)

let ima_prednost_pred x y = 
    match x.status with
    | Staff -> true
    | Passenger (prioriteta) ->
        match prioriteta with
        | Top -> y.status != Staff
        | Group (i) -> 
            match y.status with
            | Staff -> false
            | Passenger (Top) -> false
            | Passenger (Group (j)) -> j <= i

let cmp_flyer x y = 
    match x.status, y.status with
    | Staff, _ -> true
    | _, Staff -> false
    | Passenger Top, _ -> true
    | Passenger _, Passenger Top -> false
    | Passenger (Group j), Passenger (Group k) -> j > k

let vkrcavanje sez = polj_uredi sez ima_prednost_pred

(* -------- 7 -------- *)

(* 
let blokiranje sez = 
    let rec aux acc sez =  *)
