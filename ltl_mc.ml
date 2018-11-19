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
                                               
let nusmv_write_var (k:kripke): string =
  "VAR\n" ^ nusmv_write_states k ^ "\n"

let rec nusmv_write_next_state_rec (k:kripke): string =
  match k with
  | [] -> ""
  | (s,sl)::k' ->
     match sl with
     | [] -> ""
     | _ ->
        "(state=s" ^ string_of_int s ^ "): {" ^
          nusmv_write_states_rec sl ^
            "};\n" ^ nusmv_write_next_state_rec k'
                                                              
let nusmv_write_next_state (k:kripke): string =
  "next(state) :=\ncase\n" ^ nusmv_write_next_state_rec k ^ "TRUE: state;\nesac;\n"

let nusmv_write_init_state (init:state): string =
  "init(state) := s" ^ string_of_int init ^ ";\n"
                                              
let rec nusmv_write_list_states (l:state list): string =
  match l with
  | [] -> ""
  | s::[] -> "(state=s" ^ string_of_int s ^ ");"
  | s::l' -> "(state=s" ^ string_of_int s ^ ") | " ^ nusmv_write_list_states l'
                                              
let rec nusmv_write_atp (m:marking): string =
  match m with
  | [] -> ""
  | (a,l)::m' ->
     match l with
     | [] -> "p" ^ string_of_int a ^ " := FALSE;\n" ^ nusmv_write_atp m'
     | _ -> "p" ^ string_of_int a ^ " := " ^
              nusmv_write_list_states l ^ "\n"

let nusmv_write_define (m:marking): string =
  "DEFINE\n" ^ nusmv_write_atp m
                                                              
let nusmv_write_assign (k:kripke) (init:state): string =
  "ASSIGN\n" ^ nusmv_write_init_state init ^ nusmv_write_next_state k 

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
  "MODULE main\n" ^
    nusmv_write_var k ^
      nusmv_write_assign k init ^
        nusmv_write_define m ^ 
        "LTLSPEC\n" ^ nusmv_write_spec spec
                                                                         
(* LTL model-Checking. Takes a model, an initial state, a marking and a specification *)
let ltl_mc (k:kripke) (init:state) (m:marking) (spec:ltl): bool =
  let file = open_out "ltl" in
  let _ = Printf.fprintf file "%s" (nusmv_write_pbm k init m spec) in
  close_out file;
  let _ = Sys.command ("./NuSMV -dcx ltl > output") in
  true
  
