(* Common types *)

(* Atomic propositions *)
type atp = int

(* Obervational powers for epistemic logics *)
type observation = int

(* States *)
type state = I of int
           | A of int * int list * observation (* for augmented kripke models *)

(* Writing states for NuSMV *)
let rec string_of_I (i:int list): string =
  match i with
  | [] -> ""
  | s::i' -> "I" ^ string_of_int s ^ string_of_I i'
                                  
let string_of_state (s:state): string =
  match s with
  | I n -> "s" ^ string_of_int n
  | A (s,i,o) -> "s" ^ string_of_int s ^
                   string_of_I i ^
                     string_of_int o
              
(* Writing Atomic Props for NuSMV *)
let string_of_atp (a:atp): string =
  "p" ^ string_of_int a
                                         
(* Writing Observations for NuSMV *)
let string_of_obs (o:observation): string =
  "o" ^ string_of_int o
