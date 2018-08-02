open Int64;;
open Const;;
open Utils;;
open Flip;;
open Eval;;

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
