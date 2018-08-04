open Printf;;
open Hashtbl;;
open Utils;;
open Int64;;
open Search;;
open Const;;

open Joseki

let simulation_score = ref 0;;
let simulation_counter = ref 0;;

let translate (myboard,opboard) ans : unit =
  let stones = int64_popcount (logor myboard opboard) in
  if stones < 13 || stones > 51 then ()
  else
  (simulation_counter := !simulation_counter + 1;
  if !simulation_counter mod 10000 = 0 then
      (print_int !simulation_score;
      print_string " / ";
      print_int !simulation_counter;
      print_string "\n";
      flush_all ()
      )
  else ();
  let best = deep_search myboard opboard search_depth in
  let (my_x,my_y) = snd best in
  if my_x*8 + my_y = ans then simulation_score := !simulation_score + 1
  else ())

let main () =
  loading_book ();
  print_string "loaded\n";
  flush_all ();
  Hashtbl.iter translate joseki_table;
  print_int !simulation_score;
  print_string " / ";
  print_int !simulation_counter;
  print_string "\n";;

main ()
