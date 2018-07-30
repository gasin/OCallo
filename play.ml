open Int64;;
open Array;;
open Color;;
open Command;;

exception End_loop;;

type board = color array array;;

let search_depth = 4
let last_search_depth = 18

let iinf = 1000000000000000;;
let inf  = 1000000000000;;

let int64_get board i : bool =
  equal (logand (shift_right_logical board i) 0x1L) 0x1L;;

let int64_flip board i : int64 =
  logxor board (shift_left 0x1L i);;

let int64_popcount x : int =
  let m1  = 0x5555555555555555L in
  let m2  = 0x3333333333333333L in
  let m4  = 0x0f0f0f0f0f0f0f0fL in
  let h01 = 0x0101010101010101L in
  let x = Int64.sub x (logand (shift_right_logical x 1) m1) in
  let x = Int64.add (logand x m2) (logand (shift_right_logical x 2) m2) in
  let x = logand (Int64.add x (shift_right_logical x 4)) m4 in
  to_int (shift_right_logical (Int64.mul x h01) 56);;

let int64_nlz x : int =
  let x = logor x (shift_right_logical x 1) in
  let x = logor x (shift_right_logical x 2) in
  let x = logor x (shift_right_logical x 4) in
  let x = logor x (shift_right_logical x 8) in
  let x = logor x (shift_right_logical x 16) in
  let x = logor x (shift_right_logical x 32) in
  int64_popcount (lognot x);;

let bitboard_flip pos myboard opboard : int64 =
  let mask = shift_right_logical 0x0080808080808080L (63 - pos) in
  (*let outflank = logand (shift_right_logical 0x8000000000000000L (int64_nlz (logand (lognot opboard) mask))) myboard in*)
  let outflank = logand (shift_right_logical (int64_flip 0x0L 63) (int64_nlz (logand (lognot opboard) mask))) myboard in
  let flipped = logand (shift_left (neg outflank) 1) mask in
  let mask = shift_left 0x0101010101010100L pos in
  let outflank = logand (logand mask (Int64.add (logor opboard (lognot mask)) 0x1L)) myboard in
  let ret = logor flipped (logand (Int64.sub outflank (if equal outflank 0x0L then 0x0L else 0x1L)) mask) in

  let opboard = logand opboard 0x7e7e7e7e7e7e7e7eL in
  let mask = shift_right_logical 0x7f00000000000000L (63 - pos) in
  (*let outflank = logand (shift_right_logical 0x8000000000000000L (int64_nlz (logand (lognot opboard) mask))) myboard in*)
  let outflank = logand (shift_right_logical (int64_flip 0x0L 63) (int64_nlz (logand (lognot opboard) mask))) myboard in
  let flipped  = logand (shift_left (neg outflank) 1) mask in
  let mask = shift_left 0x00000000000000feL pos in
  let outflank = logand (logand mask (Int64.add (logor opboard (lognot mask)) 0x1L)) myboard in
  let flipped = logor flipped (logand (Int64.sub outflank (if equal outflank 0x0L then 0x0L else 0x1L)) mask) in
  let ret = logor ret flipped in

  let mask = shift_right_logical 0x0102040810204000L (63 - pos) in
  (*let outflank = logand (shift_right_logical 0x8000000000000000L (int64_nlz (logand (lognot opboard) mask))) myboard in*)
  let outflank = logand (shift_right_logical (int64_flip 0x0L 63) (int64_nlz (logand (lognot opboard) mask))) myboard in
  let flipped  = logand (shift_left (neg outflank) 1) mask in
  let mask = shift_left 0x0002040810204080L pos in
  let outflank = logand (logand mask (Int64.add (logor opboard (lognot mask)) 0x1L)) myboard in
  let flipped = logor flipped (logand (Int64.sub outflank (if equal outflank 0x0L then 0x0L else 0x1L)) mask) in
  let ret = logor ret flipped in

  let mask = shift_right_logical 0x0040201008040201L (63 - pos) in
  (*let outflank = logand (shift_right_logical 0x8000000000000000L (int64_nlz (logand (lognot opboard) mask))) myboard in*)
  let outflank = logand (shift_right_logical (int64_flip 0x0L 63) (int64_nlz (logand (lognot opboard) mask))) myboard in
  let flipped  = logand (shift_left (neg outflank) 1) mask in
  let mask = shift_left (int64_flip 0x0040201008040200L 63) pos in
  let outflank = logand (logand mask (Int64.add (logor opboard (lognot mask)) 0x1L)) myboard in
  let flipped = logor flipped (logand (Int64.sub outflank (if equal outflank 0x0L then 0x0L else 0x1L)) mask) in
  let ret = logor ret flipped in ret;;

let bitboard_put pos myboard opboard : bool =
  equal (bitboard_flip pos myboard opboard) 0x0L = false;;

let count board : int =
  int64_popcount board;;

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
  print_endline "  (X: Black,  O: White)";;

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

let dirs = [ (-1,-1); (0,-1); (1,-1); (-1,0); (1,0); (-1,1); (0,1); (1,1) ];;

let rec get_list_max l i : (int * int) =
  match l with
  | []    -> (-100000000,-1)
  | s::ts -> let p = get_list_max ts (i+1) in if s > (fst p) then (s, i) else p;;

let flippable_indices_line myboard opboard (di,dj) (i,j) : int64 =
  if i > 7 || i < 0 || j > 7 || j < 0 || (int64_get opboard (i*8+j) = false) then
    0x0L
  else
    let rec g (di,dj) (i,j) ret =
    if i > 7 || i < 0 || j > 7 || j < 0 then
      0x0L
    else if (int64_get opboard (i*8+j)) then
      g (di,dj) (i+di,j+dj) (int64_flip ret (i*8+j))
    else if (int64_get myboard (i*8+j)) then
      ret
    else
      0x0L in
      g (di,dj) (i+di,j+dj) (int64_flip 0x0L (i*8+j));;

let flippable_indices myboard opboard (i,j) : int64 =
  (*bitboard_flip (i*8+j) myboard opboard;;*)
  List.fold_left (fun x (di,dj) -> logor x (flippable_indices_line myboard opboard (di,dj) (i+di,j+dj))) 0x0L dirs;;

let flippable_indices_count_line myboard opboard (di,dj) (i,j) : int =
  if i > 7 || i < 0 || j > 7 || j < 0 || (int64_get opboard (i*8+j) = false) then
    0
  else
    let rec g (di,dj) (i,j) ret =
    if i > 7 || i < 0 || j > 7 || j < 0 then
      0
    else if (int64_get opboard (i*8+j)) then
      g (di,dj) (i+di,j+dj) (ret+1)
    else if (int64_get myboard (i*8+j)) then
      ret
    else
      0 in
      g (di,dj) (i+di,j+dj) 1;;

let flip_count myboard opboard (i,j) : int =
  List.fold_left (fun x (di,dj) -> x + (flippable_indices_count_line myboard opboard (di,dj) (i+di,j+dj))) 0 dirs;;

let is_valid_move myboard opboard (i,j) : bool =
  (int64_get (logor myboard opboard) (i*8+j) = false) && (flip_count myboard opboard (i,j) > 0);;
  (*(int64_get (logor !myboard opboard) (i*8+j) = false) && (bitboard_put (i*8+j) !myboard opboard);;*)

let empty_count myboard opboard : int =
  64 - (int64_popcount (logor myboard opboard));;

let mix xs ys =
  List.concat (List.rev_map (fun x -> List.rev_map (fun y -> (x,y)) ys) xs);;

let valid_moves myboard opboard =
  let ls = [0;1;2;3;4;5;6;7] in
  List.filter (is_valid_move myboard opboard)
    (mix ls ls);;

let cell_value_list = [|  60;-20;  0; -1; -1;  0;-20; 60;
                         -20;-30; -3; -3; -3; -3;-30;-20;
                           0; -3;  0; -1; -1;  0; -3;  0;
                          -1; -3; -1; -1; -1; -1; -3; -1;
                          -1; -3; -1; -1; -1; -1; -3; -1;
                           0; -3;  0; -1; -1;  0; -3;  0;
                         -20;-30; -3; -3; -3; -3;-30;-20;
                          60;-20;  0; -1; -1;  0;-20; 60 |];;

let last_eval_board myboard opboard : int =
  (int64_popcount myboard) - (int64_popcount opboard);;

let sub_fast_search myboard opboard ms =
  List.map (fun (i,j) ->
    let flip_cells = flippable_indices myboard opboard (i,j) in
    (*let flip_cells = bitboard_flip (i*8+j) !myboard opboard in*)
    let new_myboard = logxor (int64_flip myboard (i*8+j)) flip_cells in
    let new_opboard = logxor opboard flip_cells in
    let ret = List.length (valid_moves new_opboard new_myboard) in
    (ret, (i,j))
  ) ms;;

let rec last_update_board myboard opboard best ms : (int * (int * int)) =
  if List.length ms = 0 then best else (let (i,j) = List.hd ms in
  let flip_cells = flippable_indices myboard opboard (i,j) in
  (*let flip_cells = bitboard_flip (i*8+j) !myboard opboard in*)
    let new_myboard = logxor (int64_flip myboard (i*8+j)) flip_cells in
    let new_opboard = logxor opboard flip_cells in
    let ret : int = -1*(fst (last_deep_search new_opboard new_myboard false)) in
    if ret > 0 then (ret, (i,j))
    else last_update_board myboard opboard (if (fst best) < ret then (ret, (i,j)) else best) (List.tl ms))

 and last_deep_search myboard opboard again: (int * (int * int)) =
  let ms = valid_moves myboard opboard in
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
    (last_update_board myboard opboard best fast_ms)
  else
    let best = (-iinf, (-1,-1)) in
    (last_update_board myboard opboard best ms)

let eval_board myboard opboard : int =
(*
  let k = Random.int 3 in
  let value = ref (k-1) in
*)
  let value = ref 0 in
  let ms = valid_moves opboard myboard in
  ((if (int64_get (logor myboard opboard) 0) then
    (cell_value_list.(1) <- cell_value_list.(1) + 30;
    cell_value_list.(8) <- cell_value_list.(8) + 30;
    cell_value_list.(9) <- cell_value_list.(9) + 30)
  else ());
  (if (int64_get (logor myboard opboard) 7) then
    (cell_value_list.(6) <- cell_value_list.(6) + 30;
    cell_value_list.(15) <- cell_value_list.(15) + 30;
    cell_value_list.(14) <- cell_value_list.(14) + 30)
  else ());
  (if (int64_get (logor myboard opboard) 56) then
    (cell_value_list.(57) <- cell_value_list.(57) + 30;
    cell_value_list.(48) <- cell_value_list.(48) + 30;
    cell_value_list.(49) <- cell_value_list.(49) + 30)
  else ());
  (if (int64_get (logor myboard opboard) 63) then
    (cell_value_list.(62) <- cell_value_list.(62) + 30;
    cell_value_list.(55) <- cell_value_list.(55) + 30;
    cell_value_list.(54) <- cell_value_list.(54) + 30)
  else ());
  for i=0 to 63 do
    value := !value + (if (int64_get myboard i) then cell_value_list.(i) else if (int64_get opboard i) then -1*cell_value_list.(i) else 0)
  done;
  (if (int64_get (logor myboard opboard) 0) then
    (cell_value_list.(1) <- cell_value_list.(1) - 30;
    cell_value_list.(8) <- cell_value_list.(8) - 30;
    cell_value_list.(9) <- cell_value_list.(9) - 30)
  else ());
  (if (int64_get (logor myboard opboard) 7) then
    (cell_value_list.(6) <- cell_value_list.(6) - 30;
    cell_value_list.(15) <- cell_value_list.(15) - 30;
    cell_value_list.(14) <- cell_value_list.(14) - 30)
  else ());
  (if (int64_get (logor myboard opboard) 56) then
    (cell_value_list.(57) <- cell_value_list.(57) - 30;
    cell_value_list.(48) <- cell_value_list.(48) - 30;
    cell_value_list.(49) <- cell_value_list.(49) - 30)
  else ());
  (if (int64_get (logor myboard opboard) 63) then
    (cell_value_list.(62) <- cell_value_list.(62) - 30;
    cell_value_list.(55) <- cell_value_list.(55) - 30;
    cell_value_list.(54) <- cell_value_list.(54) - 30)
  else ());
  !value - (List.length ms) * 5);;

let rec update_board myboard opboard depth prebest best ms : (int * (int * int)) =
  if List.length ms = 0 then best else (let (i,j) = List.hd ms in
  let flip_cells = flippable_indices myboard opboard (i,j) in
  (*let flip_cells = bitboard_flip (i*8+j) !myboard opboard in*)
  let new_myboard = logxor (int64_flip myboard (i*8+j)) flip_cells in
  let new_opboard = logxor opboard flip_cells in
  let ret : int = if depth > 0 then -1*(fst (deep_search new_opboard new_myboard (fst best) (depth-1)))
                  else eval_board new_myboard new_opboard in
  if prebest >= -1*ret then (ret, (i,j))
  else update_board myboard opboard depth prebest (if fst best < ret then (ret, (i,j)) else best) (List.tl ms))

 and deep_search myboard opboard prebest depth : (int * (int * int)) =
  let ms = valid_moves myboard opboard in
  if List.length ms = 0 then
    if int64_popcount myboard = 0 then (-inf, (-1,-1))
    else ((if depth > 0 then -1*(fst (deep_search opboard myboard iinf (depth-1))) else eval_board myboard opboard), (-1,-1))
  else if depth > 2 then
    let fast_ms = List.map (fun x -> (snd x)) (List.sort (fun x y -> (fst x) - (fst y)) (sub_fast_search myboard opboard ms)) in
    let best = (-iinf, (-1,-1)) in
    (update_board myboard opboard depth prebest best fast_ms)
  else
    let best = (-iinf, (-1,-1)) in
    (update_board myboard opboard depth prebest best ms)

let play board color =
  let ocolor  = opposite_color color in
  let myboard = convert_board board color in
  let opboard = convert_board board ocolor in
  let emp = empty_count myboard opboard in
  print_bit_board myboard opboard;
  let ms = valid_moves myboard opboard in
    if ms = [] then
      Pass
    else
      if (emp <= last_search_depth) then
        let best = last_deep_search myboard opboard false in
        Mv (((fst (snd best))+1), ((snd (snd best))+1))
      else
        let best = deep_search myboard opboard (-1*iinf) search_depth in
        Mv (((fst (snd best))+1), ((snd (snd best))+1));;

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

