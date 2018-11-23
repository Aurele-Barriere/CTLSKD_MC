open Common
open Models
open Logics
open Ltl_mc
open Ctls_mc


let rec generate_spec_ctls (n:int): state_ctls =
  match n with
  | 0 -> ST_CTLS_E (P_CTLS_U (P_CTLS_S ST_CTLS_TRUE, P_CTLS_S (ST_CTLS_AP 1)))
  | _ -> ST_CTLS_E (P_CTLS_X (P_CTLS_S (generate_spec_ctls (n-1))))
       

let rec new_state (n:int): state list =
  match n with
  | 0 -> [I 0]
  | _ -> begin match n mod 2 with
         | 0 -> (I n)::(new_state (n-2))
         | _ -> new_state(n-1)
         end
              
let rec generate_kripke_ctls (n:int): kripke =
  match n with
  | 0 -> [(I 0,[I 0])]
  | _ -> let odd = (2*n) - 1 in
         let even = 2*n in
         (I odd,[I odd]) :: ((I even, new_state odd) :: generate_kripke_ctls (n-1))
              
       
let main =
  let formula_size = int_of_string(Sys.argv.(1)) in
  let kripke_size = int_of_string(Sys.argv.(2)) in
  let _ = print_endline "CTLS Tests" in
  let k = generate_kripke_ctls kripke_size in
  let s = generate_spec_ctls formula_size in
  let init = I (2 * kripke_size) in
  let m = [(1, [I 0])] in
  let result = ctls_mc k init m s in
  match result with
  | true -> print_endline "test_passed"
  | false -> print_endline "test_failed"
                      
