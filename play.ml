open Int64;;
open Array;;
open Color;;
open Command;;
open Hashtbl;;
open Const;;
open Utils;;
open Flip;;
open Eval;;

exception End_loop;;

type board = color array array;;

let rec get_empty_area (board : int64) (i : int) : (int * int) list =
  if i = 64 then []
  else if int64_get board i = false then (((i lsr 3), (i land 7)) :: (get_empty_area board (i+1)))
  else (get_empty_area board (i+1))

let convert_board board color : int64 =
  let ret = ref 0x0L in
  for i=1 to 8 do
    for j=1 to 8 do
      if board.(i).(j) = color then ret := int64_flip !ret ((i-1)*8+(j-1))
    done
  done;
  !ret;;

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

let print_bit_board myboard opboard =
  print_endline " |A B C D E F G H ";
  print_endline "-+----------------";
  for j=0 to 7 do
    print_int j; print_string "|";
    for i=0 to 7 do
      if int64_get myboard (i*8+j) then print_string "X"
      else if int64_get opboard (i*8+j) then print_string "O"
      else print_string " ";
      print_string " "
    done;
    print_endline ""
  done;
  print_endline "  (X: Me,  O: Op)";;

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


let sub_fast_search (myboard : int64) (opboard : int64) (ms : (int * int) list) : (int * (int * int)) list =
  List.map (fun (i,j) ->
    let flip_cells = flippable_indices myboard opboard (i,j) in
    (*let flip_cells = bitboard_flip (i*8+j) !myboard opboard in*)
    let new_myboard = logxor (int64_flip myboard (i*8+j)) flip_cells in
    let new_opboard = logxor opboard flip_cells in
    let ret = (eval_board new_opboard new_myboard) in
    (ret, (i,j))
  ) ms;;

let rec last_update_board (myboard : int64) (opboard : int64) (emp : int) (best : (int * (int * int))) (ms : (int * int) list) : (int * (int * int)) =
  if List.length ms = 0 then best else (let (i,j) = List.hd ms in
  let flip_cells = flippable_indices myboard opboard (i,j) in
  (*let flip_cells = bitboard_flip (i*8+j) !myboard opboard in*)
    let new_myboard = logxor (int64_flip myboard (i*8+j)) flip_cells in
    let new_opboard = logxor opboard flip_cells in
    (Hashtbl.remove empty_cells (i,j);
    let ret : (int * (int * int)) = (last_deep_search new_opboard new_myboard false) in
    (Hashtbl.add empty_cells (i,j) ();
    let ret2 = -1 * (fst ret) in
    (if emp > 6 then Hashtbl.add last_hash_table (new_opboard, new_myboard) ret else ();
    if ret2 > 0 then (ret2, (i,j))
    else last_update_board myboard opboard emp (if (fst best) < ret2 then (ret2, (i,j)) else best) (List.tl ms)))))

 and last_deep_search (myboard : int64) (opboard : int64) (again : bool) : (int * (int * int)) =
  try (Hashtbl.find last_hash_table (myboard, opboard)) with Not_found ->
  (let ms = new_valid_moves myboard opboard in
  let emp = empty_count myboard opboard in
  if emp = 0 then
    (last_eval_board myboard opboard, (-1, -1))
  else if List.length ms = 0 then
    (if int64_popcount myboard = 0 then (-inf, (-1, -1))
    else if again then (last_eval_board myboard opboard, (-1, -1))
    else (-1*(fst (last_deep_search opboard myboard true)), (-1,-1)))
  else if emp = 1 then
    let (i,j) = List.hd ms in
    let flip_cells_count = flip_count myboard opboard (i,j) in
    let ret : int = (last_eval_board myboard opboard) + 1 + flip_cells_count * 2 in
    (ret, (i, j))
  else if emp > 3 then
    let fast_ms = List.map (fun x -> (snd x)) (List.sort (fun x y -> (fst x) - (fst y)) (sub_fast_search myboard opboard ms)) in
    let best = (-iinf, (-1,-1)) in
    (last_update_board myboard opboard emp best fast_ms)
  else
    let best = (-iinf, (-1,-1)) in
    (last_update_board myboard opboard emp best ms))

let rec update_board (myboard : int64) (opboard : int64) (depth : int) (alpha : int) (beta : int) (best : (int * (int * int))) (ms : (int * int) list) : (int * (int * int)) =
  if List.length ms = 0 then best else (let (i,j) = List.hd ms in
  let flip_cells = flippable_indices myboard opboard (i,j) in
  (*let flip_cells = bitboard_flip (i*8+j) !myboard opboard in*)
  let new_myboard = logxor (int64_flip myboard (i*8+j)) flip_cells in
  let new_opboard = logxor opboard flip_cells in
  let ret : int = if depth > 0 then
                    (* let ret2 = (deep_search new_opboard new_myboard (-1 * (fst best)) (-1*alpha) (depth-1)) in
                                     (Hashtbl.add hash_table (new_opboard, new_myboard) ret2; -1 * (fst ret2)) *)
                  -1 * (fst (deep_search new_opboard new_myboard (-1 * (fst best)) (-1*alpha) (depth-1)))
                  else eval_board new_myboard new_opboard in
  if alpha <= ret then (ret, (i,j))
  else if beta > ret then update_board myboard opboard depth alpha ret (if fst best < ret then (ret, (i,j)) else best) (List.tl ms)
  else update_board myboard opboard depth alpha beta (if fst best < ret then (ret, (i,j)) else best) (List.tl ms))

 and deep_search (myboard : int64) (opboard : int64) (alpha : int) (beta : int) (depth : int) : (int * (int * int)) =
  (*try (Hashtbl.find hash_table (myboard, opboard)) with Not_found ->
  ( *)let ms = valid_moves myboard opboard in
  if List.length ms = 0 then
    if int64_popcount myboard = 0 then (-inf, (-1,-1))
    else ((if depth > 0 then -1*(fst (deep_search opboard myboard (-1*beta) (-1*alpha) (depth-1))) else eval_board myboard opboard), (-1,-1))
  else if depth > 2 then
    let fast_ms = List.map (fun x -> (snd x)) (List.sort (fun x y -> (fst x) - (fst y)) (sub_fast_search myboard opboard ms)) in
    let best = (beta, (-1,-1)) in
    (update_board myboard opboard depth alpha beta best fast_ms)
  else
    let best = (beta, (-1,-1)) in
    (update_board myboard opboard depth alpha beta best ms)

let make_empty_cells (myboard : int64) (opboard : int64) : unit =
  let board = lognot (logor myboard opboard) in
  for i=0 to 63 do
    if int64_get board i then Hashtbl.add empty_cells (i lsr 3, i land 7) () else ();
  done

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
      if (emp <= last_search_depth) then
        (make_empty_cells myboard opboard;
        let best = last_deep_search myboard opboard false in
        (print_string "decided "; print_int (fst best); print_string "\n";
         print_string "hash_table "; print_int (Hashtbl.length last_hash_table); print_string "\n";
        if fst best >= 0 then
          Mv (((fst (snd best))+1), ((snd (snd best))+1))
        else
          let best = deep_search myboard opboard iinf (-1*iinf) (search_depth-1) in
          Mv (((fst (snd best))+1), ((snd (snd best))+1))))
      else
        let best = deep_search myboard opboard iinf (-1*iinf) search_depth in
        (print_string "predict "; print_int (fst best); print_string "\n";
         (*print_string "hash_table "; print_int (Hashtbl.length hash_table); print_string "\n"; *)
        Mv (((fst (snd best))+1), ((snd (snd best))+1)))));;

let old_count board color : int =
  let s = ref 0 in
    for i=1 to 8 do
      for j=1 to 8 do
        if board.(i).(j) = color then s := !s + 1
      done
    done;
    !s;;

let old_flippable_indices_line board color (di,dj) (i,j) =
  let ocolor = opposite_color color in
  let rec f (di,dj) (i,j) r =
    if board.(i).(j) = ocolor then
      g (di,dj) (i+di,j+dj) ( (i,j) :: r )
    else
      []
  and    g (di,dj) (i,j) r =
    if board.(i).(j) = ocolor then
      g (di,dj) (i+di,j+dj) ( (i,j) :: r )
    else if board.(i).(j) = color then
      r
    else
      [] in
    f (di,dj) (i,j) [];;

let old_flippable_indices board color (i,j) =
  let bs = List.map (fun (di,dj) -> old_flippable_indices_line board color (di,dj) (i+di,j+dj)) dirs in
    List.concat bs;;

let doMove board com color =
  match com with
      GiveUp  -> board
    | Pass    -> board
    | Mv (i,j) ->
	let ms = old_flippable_indices board color (i,j) in
	let _  = List.map (fun (ii,jj) -> board.(ii).(jj) <- color) ms in
	let _  = board.(i).(j) <- color in
	  board
    | _ -> board;;

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

