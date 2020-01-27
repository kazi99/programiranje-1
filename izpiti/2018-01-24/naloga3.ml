type vektor = int * int
type matrika = int * int * int * int

module type Linearna = sig
  (* Tip linearnih preslikav *)
  type t
  (* Identiteta *)
  val id : t
  (* Dano preslikavo uporabi na vektorju *)
  val uporabi : t -> vektor -> vektor
  (* Vrne linearno preslikavo, določeno z matriko *)
  val iz_matrike : matrika -> t
  (* Vrne linearno preslikavo, določeno s funkcijo
     (predpostavite lahko, da je funkcija linearna) *)
  val iz_funkcije : (vektor -> vektor) -> t
  (* Vrne kompozitum danih preslikav. *)
  val kompozitum : t -> t -> t
end

module Matrika : Linearna = struct

  type t = matrika

  let id = (1,0,0,1)

  let uporabi (a, b, c, d) (x, y) = (a * x + b * y, c * x + d * y) 

  let iz_matrike m = m

  let pi1 (x, y) = x
  let pi2 (x, y) = y

  let iz_funkcije f = (pi1 (f (1,0)), pi1 (f (0,1)), pi2 (f (1,0)), pi2 (f (0,1)) ) 

  let kompozitum (a, b, c, d) (e, f, g, h) = (a*e + b*g, a*f + b*h, c*e + d*g, c*f + d*h)

end

module Funkcija : Linearna = struct

  type t = vektor -> vektor

  let id = fun (x, y) -> (x, y)

  let uporabi f (x, y) = f (x, y)

  let iz_matrike ((a, b, c, d) : matrika) = fun ((x, y) : vektor) -> ((a*x + b*y, c*x + d*y) : vektor)

  let iz_funkcije (f : t) = f 

  let kompozitum f g = fun x -> f (g x) 
  
end

(* testi *)

let rotacija ((x, y) : vektor) = (-y, x) 

let m = Matrika.iz_funkcije rotacija

let uporaba = Matrika.uporabi m (1,2) 

let f = Funkcija.iz_matrike (1,0,1,0)

let a = Funkcija.uporabi f (100, 100)
let b = Funkcija.uporabi 