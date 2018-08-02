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
  let k = Random.int random_range in
  let value = ref (k / 2) in
  (for i=0 to 7 do
    for j=0 to 7 do
      if int64_get myboard (i*8+j) then
        value := !value + cell_value_list.(i*8+j)
      else if int64_get opboard (i*8+j) then
        value := !value - cell_value_list.(i*8+j)
      else
        (if (flip_count myboard opboard (i,j) > 0) then
          if (i,j) = (0,0) || (i,j) = (0,7) || (i,j) = (7,0) || (i,j) = (7,7) then
            value := !value + 25
          else
            value := !value + 5
         else ();
         if (flip_count opboard myboard (i,j) > 0) then
          if (i,j) = (0,0) || (i,j) = (0,7) || (i,j) = (7,0) || (i,j) = (7,7) then
            value := !value - 25
          else
            value := !value - 5
         else ())
    done
  done;
  for i=0 to 3 do
    value := !value + if (equal (logand myboard edge_mountain_sub.(i)) 0x0L && equal (logand opboard edge_mountain_sub.(i)) 0x0L) then
                        if equal (logand myboard edge_mountain.(i)) edge_mountain.(i) then 10
                        else if equal (logand opboard edge_mountain.(i)) edge_mountain.(i) then -10
                        else 0
                      else 0;
  done;
  value := !value + ((solid_stone myboard opboard corners corners_dir 0) * 10);
  !value);;
