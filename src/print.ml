open Common
open Models
       

let rec print_states (l:state list): string =
  match l with
  | [] -> ""
  | s::l' -> string_of_state s ^ " " ^ print_states l'

let rec print_std_states (l:std_state list): string =
  match l with
  | [] -> ""
  | s::l' -> string_of_int s ^ " " ^ print_std_states l'

                            
let rec print_kripke (k:kripke): unit =
  match k with
  | [] -> print_endline ""
  | (x,l)::k' -> print_endline ("state: " ^ string_of_state x );
                 print_string "successors: ";
                 print_endline (print_states l);
                 print_kripke k'

let rec print_std_kripke (k:std_kripke): unit =
  match k with
  | [] -> print_endline ""
  | (x,l)::k' -> print_endline ("state: " ^ string_of_int x );
                 print_string "successors: ";
                 print_endline (print_std_states l);
                 print_std_kripke k'
                              
let rec print_state_inf (l: (std_state * inf_set) list) =
  match l with
  | [] -> ""
  | (s,ls)::l' -> "[" ^ string_of_int s ^ " ( " ^ print_std_states ls ^ ")]\n" ^
                    print_state_inf l'
