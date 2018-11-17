(* Models: Kripke Structures and Kripke Structures with Observations *)

open Common
open List

(* A Kripke Structure is a liste of states, each associated with a list of successors *)
type kripke = (state * state list) list


(* A marking is a list of atomic propositions, each associated with a list of states where it's true *)
type marking = (atp * state list) list

(* Returns the states of the model *)
let get_states (k:kripke): state list = map fst k
                              
(* Successors of a state in a model *)
let rec successors (s:state) (k:kripke): state list =
  match k with
  | [] -> []
  | (s1,l)::k' when s1 = s -> l
  | (s1,l)::k' -> successors s k'

(* Returns all atomic propositions of a marking *)
let get_atp (m:marking): atp list = map fst m

(* Returns fresh atomic proposition *)
let rec fresh_atp_rec (a:atp) (m:marking) =
  match m with
  | [] -> a + 1
  | (a1,l)::m1 when a1 > a -> fresh_atp_rec a1 m1
  | (a1,l)::m1 -> fresh_atp_rec a m1

let fresh_atp (m:marking) = fresh_atp_rec 0 m
