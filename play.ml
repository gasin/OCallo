open Command;;
open Color;;
open Const;;
open Utils;;
open Flip;;
open Search;;
open Extra;;

let init_board () =
  let board = Array.make_matrix 10 10 none in
    for i=0 to 9 do
      board.(i).(0) <- sentinel ;
      board.(i).(9) <- sentinel ;
      board.(0).(i) <- sentinel ;
      board.(9).(i) <- sentinel ;
    done;
    board.(4).(4) <- white;
    board.(5).(5) <- white;
    board.(4).(5) <- black;
    board.(5).(4) <- black;
    board;;

let print_board board =
  print_endline " |A B C D E F G H ";
  print_endline "-+----------------";
  for j=1 to 8 do
    print_int j; print_string "|";
    for i=1 to 8 do
      print_color (board.(i).(j)); print_string " "
    done;
    print_endline ""
  done;
  print_endline "  (X: Black,  O: White)";;

let play board color =
  ((*Hashtbl.clear hash_table; *) Hashtbl.clear empty_cells;
  let ocolor  = opposite_color color in
  let myboard = convert_board board color in
  let opboard = convert_board board ocolor in
  let emp = empty_count myboard opboard in
  if color = 2 then print_string "I'm black\n"
  else print_string "I'm white\n";
  print_bit_board myboard opboard;
  let ms = valid_moves myboard opboard in
    if ms = [] then
      Pass
    else
      (if emp >= 59 then Hashtbl.clear last_hash_table else ();
      (make_empty_cells myboard opboard;
      if (emp <= last_search_depth) then
        let best = last_deep_search myboard opboard false in
        (print_string "decided "; print_int (fst best); print_string "\n";
         print_string "hash_table "; print_int (Hashtbl.length last_hash_table); print_string "\n";
        if fst best >= 0 then
          Mv (((fst (snd best))+1), ((snd (snd best))+1))
        else
          let best = deep_search myboard opboard iinf (-1*iinf) (search_depth-1) in
          Mv (((fst (snd best))+1), ((snd (snd best))+1)))
      else
        let best = deep_search myboard opboard iinf (-1*iinf) search_depth in
        (*let best = mtdf myboard opboard search_depth 0 in *)
        (print_string "predict "; print_int (fst best); print_string "\n";
         (*print_string "hash_table "; print_int (Hashtbl.length hash_table); print_string "\n"; *)
        Mv (((fst (snd best))+1), ((snd (snd best))+1))))));;

let doMove board com color =
  match com with
      GiveUp  -> board
    | Pass    -> board
    | Mv (i,j) ->
	let ms = old_flippable_indices board color (i,j) in
	let _  = List.map (fun (ii,jj) -> board.(ii).(jj) <- color) ms in
	let _  = board.(i).(j) <- color in
	  board;;

let report_result board =
  let _ = print_endline "========== Final Result ==========" in
  let bc = old_count board black in
  let wc = old_count board white in
    if bc > wc then
      print_endline "*Black wins!*"
    else if bc < wc then
      print_endline "*White wins!*"
    else
      print_endline "*Even*";
    print_string "Black: "; print_endline (string_of_int bc);
    print_string "White: "; print_endline (string_of_int wc);
    print_board board;;

