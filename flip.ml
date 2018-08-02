open Int64;;
open Array;;
open Color;;
open Hashtbl;;
open Const;;
open Utils;;

let flippable_indices_line1 (myboard : int64) (opboard : int64) ((i,j) : (int * int)) : int64 =
  if i < 0 || j < 0 || (int64_get opboard (i*8+j) = false) then
    0x0L
  else let rec g ((i,j) : (int * int)) (ret : int64) : int64 =
    if i < 0 || j < 0 then 0x0L
    else if (int64_get opboard (i*8+j)) then g (i-1,j-1) (int64_flip ret (i*8+j))
    else if (int64_get myboard (i*8+j)) then ret
    else 0x0L in g (i-1,j-1) (int64_flip 0x0L (i*8+j));;
let flippable_indices_line2 (myboard : int64) (opboard : int64) ((i,j) : (int * int)) : int64 =
  if j < 0 || (int64_get opboard (i*8+j) = false) then
    0x0L
  else let rec g ((i,j) : (int * int)) (ret : int64) : int64 =
    if j < 0 then 0x0L
    else if (int64_get opboard (i*8+j)) then g (i,j-1) (int64_flip ret (i*8+j))
    else if (int64_get myboard (i*8+j)) then ret
    else 0x0L in g (i,j-1) (int64_flip 0x0L (i*8+j));;
let flippable_indices_line3 (myboard : int64) (opboard : int64) ((i,j) : (int * int)) : int64 =
  if i > 7 || j < 0 || (int64_get opboard (i*8+j) = false) then
    0x0L
  else let rec g ((i,j) : (int * int)) (ret : int64) : int64 =
    if i > 7 || j < 0 then 0x0L
    else if (int64_get opboard (i*8+j)) then g (i+1,j-1) (int64_flip ret (i*8+j))
    else if (int64_get myboard (i*8+j)) then ret
    else 0x0L in g (i+1,j-1) (int64_flip 0x0L (i*8+j));;
let flippable_indices_line4 (myboard : int64) (opboard : int64) ((i,j) : (int * int)) : int64 =
  if i < 0 || (int64_get opboard (i*8+j) = false) then
    0x0L
  else let rec g ((i,j) : (int * int)) (ret : int64) : int64 =
    if i < 0 then 0x0L
    else if (int64_get opboard (i*8+j)) then g (i-1,j) (int64_flip ret (i*8+j))
    else if (int64_get myboard (i*8+j)) then ret
    else 0x0L in g (i-1,j) (int64_flip 0x0L (i*8+j));;
let flippable_indices_line5 (myboard : int64) (opboard : int64) ((i,j) : (int * int)) : int64 =
  if i > 7 || (int64_get opboard (i*8+j) = false) then
    0x0L
  else let rec g ((i,j) : (int * int)) (ret : int64) : int64 =
    if i > 7 then 0x0L
    else if (int64_get opboard (i*8+j)) then g (i+1,j) (int64_flip ret (i*8+j))
    else if (int64_get myboard (i*8+j)) then ret
    else 0x0L in g (i+1,j) (int64_flip 0x0L (i*8+j));;
let flippable_indices_line6 (myboard : int64) (opboard : int64) ((i,j) : (int * int)) : int64 =
  if i < 0 || j > 7 || (int64_get opboard (i*8+j) = false) then
    0x0L
  else let rec g ((i,j) : (int * int)) (ret : int64) : int64 =
    if i < 0 || j > 7 then 0x0L
    else if (int64_get opboard (i*8+j)) then g (i-1,j+1) (int64_flip ret (i*8+j))
    else if (int64_get myboard (i*8+j)) then ret
    else 0x0L in g (i-1,j+1) (int64_flip 0x0L (i*8+j));;
let flippable_indices_line7 (myboard : int64) (opboard : int64) ((i,j) : (int * int)) : int64 =
  if j > 7 || (int64_get opboard (i*8+j) = false) then
    0x0L
  else let rec g ((i,j) : (int * int)) (ret : int64) : int64 =
    if j > 7 then 0x0L
    else if (int64_get opboard (i*8+j)) then g (i,j+1) (int64_flip ret (i*8+j))
    else if (int64_get myboard (i*8+j)) then ret
    else 0x0L in g (i,j+1) (int64_flip 0x0L (i*8+j));;
let flippable_indices_line8 (myboard : int64) (opboard : int64) ((i,j) : (int * int)) : int64 =
  if i > 7 || j > 7 || (int64_get opboard (i*8+j) = false) then
    0x0L
  else let rec g ((i,j) : (int * int)) (ret : int64) : int64 =
    if i > 7 || j > 7 then 0x0L
    else if (int64_get opboard (i*8+j)) then g (i+1,j+1) (int64_flip ret (i*8+j))
    else if (int64_get myboard (i*8+j)) then ret
    else 0x0L in g (i+1,j+1) (int64_flip 0x0L (i*8+j));;


let flippable_indices (myboard : int64) (opboard : int64) ((i,j) : (int * int)) : int64 =
(*
  let ind = ref 0 in
  List.fold_left (fun ret mask ->
                 (try Hashtbl.find flip_table ((i*8+j), ((logand myboard mask), (logand opboard mask)))
                  with Not_found ->
                  (let v : int64 = List.fold_left (fun (x : int64) ((di,dj) : (int * int)) -> logor x (flippable_indices_line myboard opboard (di,dj) (i+di,j+dj))) 0x0L dirs_array.(!ind) in
                 (Hashtbl.add flip_table ((i*8+j), ((logand myboard mask), (logand opboard mask))) v; ind := !ind+1; v))))
                  0x0L flip_mask.(i*8+j);;
*)
  (*bitboard_flip (i*8+j) myboard opboard;;*)
  (*List.fold_left (fun x (di,dj) -> logor x (flippable_indices_line myboard opboard (di,dj) (i+di,j+dj))) 0x0L dirs;;*)
  let ret = ref 0x0L in
  (ret := logor!ret (flippable_indices_line1 myboard opboard (i-1,j-1));
  ret := logor!ret (flippable_indices_line2 myboard opboard (i,j-1));
  ret := logor!ret (flippable_indices_line3 myboard opboard (i+1,j-1));
  ret := logor!ret (flippable_indices_line4 myboard opboard (i-1,j));
  ret := logor!ret (flippable_indices_line5 myboard opboard (i+1,j));
  ret := logor!ret (flippable_indices_line6 myboard opboard (i-1,j+1));
  ret := logor!ret (flippable_indices_line7 myboard opboard (i,j+1));
  ret := logor!ret (flippable_indices_line8 myboard opboard (i+1,j+1));
  !ret)

let flippable_indices_count_line (myboard : int64) (opboard : int64) ((di,dj) : (int * int)) ((i,j) : (int * int)) : int =
  if i > 7 || i < 0 || j > 7 || j < 0 || (int64_get opboard (i*8+j) = false) then
    0
  else
    let rec g ((di,dj) : (int * int)) ((i,j) : (int * int)) (ret : int) : int =
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

let empty_count (myboard : int64) (opboard : int64) : int =
  64 - (int64_popcount (logor myboard opboard));;

let mix xs ys =
  List.concat (List.rev_map (fun x -> List.rev_map (fun y -> (x,y)) ys) xs);;

let valid_moves (myboard : int64) (opboard : int64) : (int * int) list =
  let ls = [0;1;2;3;4;5;6;7] in
  List.filter (is_valid_move myboard opboard)
    (mix ls ls);;

let new_valid_moves (myboard : int64) (opboard : int64) : (int * int) list =
  Hashtbl.fold (fun x y z -> if is_valid_move myboard opboard x then x::z else z) empty_cells []
