(* 
Izpiti

- 2019-08-27
    • gnezdeni seznami
        List.fold_left 
        List.map
    • Mortimer mesta python 
        ne delat rekurzije na seznamih ker jih lru_cache
        ne mara (unhasable)

- 2019-06-27
    • zapisni tip type complex = { re: float; im: float } 
      (08-modularnost vec o tem)
    • Vrtovi List.fold_left
    • Tovorne ladje in zabojniki

- 2019-01-24
    • multi-mnozice 
    • Žabji pobeg iz močvirja

- 2018-08-28
    • monotona pot na drevesih Ocaml
    • verige filtrov

- 2018-07-06
    • neprazni seznami
    • simetricne zapestnice python
        funkcije kot argumenti v pythonu

- 2018-01-24
    • filtracijska drevesa
    • moduli linearnih preslikav v Z^2
    • lisjaček v sadovnjaku

 *)

(* eksplicitno podat domeno funkciji *)
let f (x : int) = x + 1

(* fancy funkcija *)
let g = fun x -> f x

(* 06-definicije-tipov : stvari s tipi *)
    type euro = Euro of float  
    (* Euro je konstruktor tipa euro *)

    (* to je zapisni tip *)
    type wizard = {name: string; age: int} 

    (* parametricni tip, parameter je 'a *)
    type 'a drevo = Drevo of 'a * ('a drevo) list 

(* 08-iskalna-drevesa : implementacija slovarjev z drevesi *)
    type 'a tree = 
    | Empty
    | Nond of 'a tree * 'a * 'a tree

    type ('key, 'value) dict = ('key * 'value) tree

(* 09-modularnost *)
    module type SIGNATURA = sig

      type t 
      val zero : t
      val eq : t -> t -> bool

    end

     module Implementacija : SIGNATURA = struct

      type t = int
      let zero = 0
      let (eq : t -> t -> bool) = (=)

    end
    
(* 12-dinamicno-programiranje 
    • miska & sir v matriki
    • alternating towers in uporaba medsebojno odvisnih funkcij 
      v Ocaml-u in python-u
    *)

(* 13-memoizacija 
    • robotkov pobeg (dodatne_vaje.py)
*)

(* 
    - Lists in Ocaml
        https://caml.inria.fr/pub/docs/manual-ocaml/libref/List.html 
    - Random in Ocaml
        https://caml.inria.fr/pub/docs/manual-ocaml/libref/Random.html        
*)
