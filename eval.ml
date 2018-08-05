open Int64;;
open Const;;
open Utils;;
open Flip;;

let last_eval_board (myboard : int64) (opboard : int64) : int =
  (int64_popcount myboard) - (int64_popcount opboard);;

let rec solid_stone_line_sub board dep (i,j) (di,dj) x : int =
  if dep = 0 then x
  else if int64_get board ((i+di)*8+j+dj) then solid_stone_line_sub board (dep-1) (i+di,j+dj) (di,dj) (x+1)
  else x

let solid_stone_line board (i,j) dirs : int =
  List.fold_left (fun x (di,dj) -> (solid_stone_line_sub board 6 (i,j) (di,dj) x)) 0 dirs

let rec solid_stone myboard opboard (corners : (int * int) list) (corners_dir : (int * int) list list) (ret : int) : int =
  if corners = [] then ret else
  let (i,j) = List.hd corners in
    if int64_get myboard (i*8+j) then
      let tmp = (solid_stone_line myboard (i,j) (List.hd corners_dir)) in
      solid_stone myboard opboard (List.tl corners) (List.tl corners_dir) (ret+tmp)
    else if int64_get opboard (i*8+j) then
      let tmp = (solid_stone_line opboard (i,j) (List.hd corners_dir)) in
      solid_stone myboard opboard (List.tl corners) (List.tl corners_dir) (ret-tmp)
    else (solid_stone myboard opboard (List.tl corners) (List.tl corners_dir) ret)

let eval_board (myboard : int64) (opboard : int64) : int =
  let k = if random_range = 0 then 0 else Random.int random_range in
  let value = ref (k / 2) in
  let my_low_board  = (to_int myboard) land 0xffffffff in
  let my_high_board = (to_int (shift_right_logical myboard 32)) land 0xffffffff in
  let op_low_board  = (to_int opboard) land 0xffffffff in
  let op_high_board = (to_int (shift_right_logical opboard 32)) land 0xffffffff in
  let stone_num = int64_popcount (logor myboard opboard) in
  let this_stage = ref 0 in
  let mycorner_mask = ref 0 in
  let opcorner_mask = ref 0 in
  (
  for i=0 to 5 do
    if stage_sepa.(i) < stone_num then
      this_stage := i+1
    else ()
  done;
  for i=0 to 3 do
    for j=0 to 7 do
      if int_get my_low_board (i*8+j) then
        value := !value + cell_value_list.(i*8+j)
      else if int_get op_low_board (i*8+j) then
        value := !value - cell_value_list.(i*8+j)
      else
        (if (flip_count myboard opboard (i,j) > 0) then
          if (i,j) = (0,0) || (i,j) = (0,7) then
            value := !value + next_put_corner_weight.(!this_stage)
          else
            value := !value + next_put_weight.(!this_stage)
         else ();
         if (flip_count opboard myboard (i,j) > 0) then
          if (i,j) = (0,0) || (i,j) = (0,7) then
            value := !value - next_put_corner_weight.(!this_stage)
          else
            value := !value - next_put_weight.(!this_stage)
         else ())
    done
  done;
  for i=4 to 7 do
    for j=0 to 7 do
      if int_get my_high_board ((i-4)*8+j) then
        value := !value + cell_value_list.(i*8+j)
      else if int_get op_high_board ((i-4)*8+j) then
        value := !value - cell_value_list.(i*8+j)
      else
        (if (flip_count myboard opboard (i,j) > 0) then
          if (i,j) = (7,0) || (i,j) = (7,7) then
            value := !value + next_put_corner_weight.(!this_stage)
          else
            value := !value + next_put_weight.(!this_stage)
         else ();
         if (flip_count opboard myboard (i,j) > 0) then
          if (i,j) = (7,0) || (i,j) = (7,7) then
            value := !value - next_put_corner_weight.(!this_stage)
          else
            value := !value - next_put_weight.(!this_stage)
         else ())
    done
  done;
  for i=0 to 3 do
    value := !value + if (equal (logand myboard edge_mountain_sub.(i)) 0x0L && equal (logand opboard edge_mountain_sub.(i)) 0x0L) then
                        if equal (logand myboard edge_mountain.(i)) edge_mountain.(i) then mountain_weight.(!this_stage)
                        else if equal (logand opboard edge_mountain.(i)) edge_mountain.(i) then -1*mountain_weight.(!this_stage)
                        else 0
                      else 0;
  done;
  for i=0 to 3 do
    (
    mycorner_mask := 0;
    opcorner_mask := 0;
    for j=0 to 3 do
      if int64_get myboard around_corner.(i).(j) then
        mycorner_mask := !mycorner_mask lor (1 lsl j)
      else if int64_get opboard around_corner.(i).(j) then
        opcorner_mask := !opcorner_mask lor (1 lsl j)
      else ()
    done;
    value := !value + corner_weight.(!this_stage).(!mycorner_mask).(!opcorner_mask)
    )
  done;

  value := !value + ((solid_stone myboard opboard corners corners_dir 0) * solid_weight.(!this_stage));
  !value);;
