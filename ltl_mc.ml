(* Model Checking LTL problems *)

open Common
open Models
open Logics


(* Writing a LTL problem in NuSMV's syntax *)
let rec nusmv_write_states_rec (l:state list): string =
  match l with
  | [] -> ""
  | s::[] -> "s" ^ string_of_int s
  | s::l' -> "s" ^ string_of_int s ^ ", " ^ nusmv_write_states_rec l'
       
let nusmv_write_states (k:kripke): string =
  "state: {" ^ nusmv_write_states_rec (get_states k) ^ "};"

let rec nusmv_write_var_rec (l:atp list): string =
  match l with
  | [] -> ""
  | a::l' -> "p" ^ string_of_int a ^ ": boolean;\n" ^ nusmv_write_var_rec l'
                                                          
let nusmv_write_var (k:kripke) (m:marking): string =
  "VAR\n" ^ nusmv_write_states k ^ "\n" ^ nusmv_write_var_rec (get_atp m)

let rec nusmv_write_next_state_rec (k:kripke): string =
  match k with
  | [] -> ""
  | (s,sl)::k' ->
     "(state=s" ^ string_of_int s ^ "): {" ^
       nusmv_write_states_rec sl ^
         "};\n" ^ nusmv_write_next_state_rec k'
                                                              
let nusmv_write_next_state (k:kripke): string =
  "next(state) :=\ncase\n" ^ nusmv_write_next_state_rec k ^ "TRUE: state;\nesac;\n"

let nusmv_write_init_state (init:state): string =
  "init(state) := s" ^ string_of_int init ^ ";\n"
                                              
let nusmv_write_init_atp (l:state list) (a:atp) (init:state): string =
  match List.mem a l with
  | true -> "init(p" ^ string_of_int a ^ ") := TRUE;\n"
  | false -> "init(p" ^ string_of_int a ^ ") := FALSE;\n"
                                            
let rec nusmv_write_next_atp_rec (l:state list): string =
  match l with
  | [] -> ""
  | s::[] -> "(state=s" ^ string_of_int s ^ ") : TRUE;"
  | s::l' -> "(state=s" ^ string_of_int s ^ ") | "
                                              
let nusmv_write_next_atp (m:marking) (init:state): string =
  match m with
  | [] -> ""
  | (a,l)::m' ->
     nusmv_write_init_atp l a init ^ 
       "next(p" ^ string_of_int a ^ ") :=\ncase\n" ^ 
         nusmv_write_next_atp_rec l ^ "\nTRUE: FALSE;\nesac;\n"
                                                              
let nusmv_write_assign (k:kripke) (init:state) (m:marking): string =
  "ASSIGN\n" ^ nusmv_write_init_state init ^
    nusmv_write_next_state k ^
      nusmv_write_next_atp m init

(* http://nusmv.fbk.eu/NuSMV/userman/v11/html/nusmv_26.html *)
let rec nusmv_write_spec (spec:ltl): string =
  match spec with
  | LTL_TRUE -> "TRUE"
  | LTL_AP a -> "p" ^ string_of_int a
  | LTL_NEG spec' -> "!(" ^ nusmv_write_spec spec' ^ ")"
  | LTL_OR (spec1,spec2) ->
     "(" ^ nusmv_write_spec spec1 ^ ") | (" ^ nusmv_write_spec spec2 ^ ")"
  | LTL_AND (spec1,spec2) -> 
     "(" ^ nusmv_write_spec spec1 ^ ") & (" ^ nusmv_write_spec spec2 ^ ")"
  | LTL_X spec' -> "X(" ^ nusmv_write_spec spec' ^ ")"
  | LTL_U (spec1,spec2) ->
     "(" ^ nusmv_write_spec spec1 ^ ") U (" ^ nusmv_write_spec spec2 ^ ")"

let nusmv_write_pbm (k:kripke) (init:state) (m:marking) (spec:ltl): string =
  nusmv_write_var k m ^
    nusmv_write_assign k init m ^
      "SPEC\n" ^ nusmv_write_spec spec
                                                                         
(* LTL model-Checking. Takes a model, an initial state, a marking and a specification *)
let ltl_mc (k:kripke) (init:state) (m:marking) (spec:ltl): bool =
  true
  
