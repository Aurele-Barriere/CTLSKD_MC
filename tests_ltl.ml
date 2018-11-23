open Common
open Models
open Logics
open Ltl_mc


let rec generate_spec_ltl (n:int): ltl =
  match n with
  | 0 -> LTL_U (LTL_TRUE, LTL_AP 1)
  | _ -> LTL_U(LTL_TRUE, LTL_X (generate_spec_ltl (n-1)))
       

let rec new_state (n:int): state list =
  match n with
  | 0 -> []
  | _ -> (I (n-1))::(new_state (n-1))
              
let rec generate_kripke_ltl (n:int): kripke =
  match n with
  | 0 -> [(I 0,[I 0])]
  | _ -> (I n,new_state n) :: generate_kripke_ltl (n-1)
              
       
let main =
  let formula_size = int_of_string(Sys.argv.(1)) in
  let kripke_size = int_of_string(Sys.argv.(2)) in
  let _ = print_endline "LTL Tests" in
  let k = generate_kripke_ltl kripke_size in
  let s = generate_spec_ltl formula_size in
  let init = I (kripke_size) in
  let m = [(1, [I 0])] in
  let result = ltl_mc k init m s in
  match result with
  | true -> print_endline "test_passed"
  | false -> print_endline "test_failed"
                      
