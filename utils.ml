open Int64;;
open Const;;

let int64_get (board : int64) (i : int) : bool =
  if i >= 32 then
    ((to_int (shift_right_logical board 32)) land (1 lsl (i-32))) > 0
  else
    ((to_int board) land (1 lsl i)) > 0;;
(*  equal (logand (shift_right_logical board i) 0x1L) 0x1L;;*)

let int64_flip (board : int64) (i : int) : int64 =
  logxor board (shift_left 0x1L i);;

let int64_popcount (x : int64) : int =
  let m1  = 0x5555555555555555L in
  let m2  = 0x3333333333333333L in
  let m4  = 0x0f0f0f0f0f0f0f0fL in
  let h01 = 0x0101010101010101L in
  let x = Int64.sub x (logand (shift_right_logical x 1) m1) in
  let x = Int64.add (logand x m2) (logand (shift_right_logical x 2) m2) in
  let x = logand (Int64.add x (shift_right_logical x 4)) m4 in
  to_int (shift_right_logical (Int64.mul x h01) 56);;

(*
let rec flip_mask_sub_sub ((di,dj) : (int * int)) ((i,j) : (int * int)) (ret : int64) : int64 =
  if i > 7 || i < 0 || j > 7 || j < 0 then
    ret
  else
    flip_mask_sub_sub (di,dj) (i+di,j+dj) (int64_flip ret (i*8+j));;

let flip_mask_sub x (di,dj) (dii,djj) : int64 =
  logor (flip_mask_sub_sub (di,dj) ((x lsr 3)+di, (x land 7)+dj) 0x0L) (flip_mask_sub_sub (dii,djj) ((x lsr 3)+dii, (x land 7)+djj) 0x0L);;

let flip_mask = Array.init 64 (fun x -> flip_mask_sub x (1,0) (-1,0)  ::
                                        flip_mask_sub x (0,1) (0,-1)  ::
                                        flip_mask_sub x (1,1) (-1,-1) ::
                                        flip_mask_sub x (1,-1) (-1,1) :: []);;
*)

(*
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
*)

let empty_count (myboard : int64) (opboard : int64) : int =
  64 - (int64_popcount (logor myboard opboard));;

let rec get_empty_area (board : int64) (i : int) : (int * int) list =
  if i = 64 then []
  else if int64_get board i = false then (((i lsr 3), (i land 7)) :: (get_empty_area board (i+1)))
  else (get_empty_area board (i+1))

let make_empty_cells (myboard : int64) (opboard : int64) : unit =
  let board = lognot (logor myboard opboard) in
  for i=0 to 63 do
    if int64_get board i then Hashtbl.add empty_cells (i lsr 3, i land 7) () else ();
  done

let convert_board board color : int64 =
  let ret = ref 0x0L in
  for i=1 to 8 do
    for j=1 to 8 do
      if board.(i).(j) = color then ret := int64_flip !ret ((i-1)*8+(j-1))
    done
  done;
  !ret;;

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

