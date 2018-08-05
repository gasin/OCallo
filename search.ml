open Int64;;
open Const;;
open Utils;;
open Flip;;
open Eval;;

let rec update_board (myboard : int64) (opboard : int64) (depth : int) (best : (int * (int * int))) (ms : (int * int) list) : (int * (int * int)) =
  if List.length ms = 0 then best else (let (i,j) = List.hd ms in
  let flip_cells = flippable_indices myboard opboard (i,j) in
  let new_myboard = logxor (int64_flip myboard (i*8+j)) flip_cells in
  let new_opboard = logxor opboard flip_cells in
  let ret = -1 * (eval_board new_opboard new_myboard) in
  update_board myboard opboard depth (if fst best < ret then (ret, (i,j)) else best) (List.tl ms))

 and deep_search (myboard : int64) (opboard : int64) (depth : int) : (int * (int * int)) =
  let ms = valid_moves myboard opboard in
  let best = (-iinf, (-1,-1)) in
  update_board myboard opboard depth best ms;;

