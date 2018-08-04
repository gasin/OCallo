open Int64;;
open Hashtbl;;
open Flip;;
open Utils;;

let joseki_table : ((int64 * int64), int) Hashtbl.t = Hashtbl.create 10000000;;

let joseki_counter = ref 0;;

let rec read_file chan (myboard, opboard) : unit =
  let a = input_char chan in
  if a = '\n' then
    (joseki_counter := !joseki_counter + 1;
    if !joseki_counter mod 10000 = 0 then
      (print_int !joseki_counter;
      print_string "\n")
    else ();
    read_file chan (0x0000000810000000L, 0x0000001008000000L))
  else
    let num = (Char.code a)-33 in
    let x = num lsr 3 in let y = num land 7 in
    (Hashtbl.add joseki_table (myboard,opboard) num;
    if (!joseki_counter mod 10000 = 0) && (empty_count myboard opboard = 30) then
      (print_bit_board myboard opboard;
      print_int x; print_string " "; print_int y; print_string "\n")
    else ();
    if (!joseki_counter mod 10000 = 0) && (empty_count myboard opboard = 31) then
      (print_bit_board myboard opboard;
      print_int x; print_string " "; print_int y; print_string "\n")
    else ();
    flush_all ();
    let flip_cells = flippable_indices myboard opboard (x,y) in
    let new_myboard = logxor (int64_flip myboard num) flip_cells in  
    let new_opboard = logxor opboard flip_cells in
    if valid_moves new_opboard new_myboard = [] then
      read_file chan (new_myboard, new_opboard)
    else
      read_file chan (new_opboard, new_myboard))

let loading_book () : unit =
  let chan = open_in "./joseki/xxx_comp.gam" in
  (try read_file chan (0x0000000810000000L, 0x0000001008000000L) with End_of_file ->
  close_in chan)
