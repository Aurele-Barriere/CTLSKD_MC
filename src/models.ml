(* Models: Kripke Structures and Kripke Structures with Observations *)

open Common
open List

(* A Kripke Structure is a liste of states, each associated with a list of successors *)
type kripke = (state * state list) list

(* Kripke structures with standard states. Input of CTLSKD problems *)
type std_kripke = (std_state * std_state list) list

(* A marking is a list of atomic propositions, each associated with a list of states where it's true *)
type marking = (atp * state list) list

(* A marking for standards Kripke Structure *)
type std_marking = (atp * std_state list) list

(* Returns the states of the kripke structure *)
let get_states k = map fst k
                              
(* Successors of a state in a model *)
let rec successors s k =
  match k with
  | [] -> []
  | (s1,l)::k' when s1 = s -> l
  | (s1,l)::k' -> successors s k'

(* Returns all atomic propositions of a marking *)
let get_atp m: atp list = map fst m

(* Returns fresh atomic proposition *)
let rec fresh_atp_rec (a:atp) m: atp =
  match m with
  | [] -> a + 1
  | (a1,l)::m1 when a1 > a -> fresh_atp_rec a1 m1
  | (a1,l)::m1 -> fresh_atp_rec a m1

let fresh_atp m: atp = fresh_atp_rec 0 m

(* A map from observation and (standard) states to values *)
module ObsMap =
  Map.Make(struct type t = (observation * std_state) let compare = compare end)

(* An observation marking om gives meaning to each observation. Looking at s1 with observations o1, you see om(o1,s1) *)
type obs_marking = int ObsMap.t                   

let eq_state (om:obs_marking) (o:observation) (s1:std_state) (s2:std_state): bool =
  ObsMap.find (o,s1) om = ObsMap.find (o,s2) om
