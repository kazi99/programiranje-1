type 'a tree = 
	| Empty
	| Node of 'a tree * 'a * 'a tree

let leaf x = Node (Empty, x, Empty)

let node = function
	| Empty -> None
	| Node (_, x, _) -> Some x

(* mono Node(l, x, r) = max (mono l, mono d, left + x + right *)

let test = Node (
	Node (
		leaf 3,
		10,
		Node (
			leaf 14,
			13,
			leaf 6
		)
	),
	11,
	Node (
		Node (
			leaf 1,
			2,
			Empty
		), 
		8,
		leaf 10
	)
)

let sub_test1 = Node (
		leaf 3,
		10,
		Node (
			leaf 14,
			13,
			leaf 6
		)
	)

let sub_test2 = Node (
		Node (
			leaf 1,
			2,
			Empty
		), 
		15,
		leaf 10
	)


(* vrne seznam ki se zacne z najvecjim elementom in pada, tj prvi node *)
(* dela prav za smer = (<=) *)
let rec decr_path (smer : 'a -> 'a -> bool) = function
	| Empty -> []
	| Node (Empty, x, Empty) -> [x]
	| Node (Node (l, y, r), x, Empty) -> 
		if smer y  x then x :: decr_path smer (Node (l, y, r))
		else [x]
	| Node (Empty, x, Node (l, y, r)) -> 
		if smer y x then x :: decr_path smer (Node (l, y, r))
		else [x]
	| Node (Node(l1, y1, r1), x, Node (l2, y2, r2)) -> 
			let a = x :: decr_path smer (Node (l1, y1, r1)) in
			let a_len = List.length a in
			let b = x :: decr_path smer (Node (l2, y2, r2)) in
			let b_len = List.length b in
		if smer y1 y2 && smer y2 x then 
			if a_len >= b_len then a else b
		else if smer y1 x && smer x y2 then a
		else if smer x y1 && smer y1 y2 then []
		else if smer y2 y1 && smer y1 x then
			if a_len >= b_len then a else b
		else if smer y2 x && smer x y1 then b
		else if smer x y2 && smer y2 y1 then []
		else []

(* 
	y1 < y2 < x
	y1 < x < y2
	x < y1 < y2

	y2 < y1 < x
	y2 < x < y1
	x < y2 < y1
 *)

let remove_first = function
	| [] -> []
	| x :: xs -> xs

(* vrne seznam ki se zacne z najmanjsim elementom in narasca, tj prvi node *)
let rec right_path_incr = function
	| Empty -> []
	| Node (_, x, Empty) -> [x]
	| Node (_, x, Node (l, y, r)) -> 
		if y > x then x :: right_path_incr (Node (l, y, r))
		else [x]

(* left to rigth increasing path *)
let rec l_to_r_incr_path tree = match tree with
	| Empty -> []
	| Node (l, x, r) -> 
		let a = l_to_r_incr_path l in
		let a_len = List.length a in
		let b = l_to_r_incr_path r in
		let b_len = List.length b in
		let c = List.rev (decr_path (<=) (Node (l, x, r))) @ [x] @ (decr_path (>=) r) in
		let c_len = List.length c in
		let m = max c_len (max a_len b_len) in
		if a_len = m then a else
		if b_len = m then b else c

