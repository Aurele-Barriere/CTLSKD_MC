(* Common types *)
open Map

(* Atomic propositions *)
type atp = int

(* Obervational powers for epistemic logics *)
type observation = int

(* A standard state is the state of a Kripke Model in the original problem *)
type std_state = int

(* An information set is the set of standards states considered possible *)
type inf_set = std_state list
                     
(* States *)
(* To define Augmented Kripke Models, we present a broader definition of states *)
type state = I of std_state
           | A of std_state * inf_set * observation

(* Writing states for NuSMV *)
let rec string_of_I (i:inf_set): string =
  match i with
  | [] -> ""
  | s::i' -> "I" ^ string_of_int s ^ string_of_I i'
              
(* Writing Atomic Props for NuSMV *)
let string_of_atp (a:atp): string =
  "p" ^ string_of_int a
                                         
(* Writing Observations for NuSMV *)
let string_of_obs (o:observation): string =
  "o" ^ string_of_int o

let string_of_state (s:state): string =
  match s with
  | I n -> "s" ^ string_of_int n
  | A (s,i,o) -> "s" ^ string_of_int s ^
                   string_of_I i ^
                     string_of_obs o
