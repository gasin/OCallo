open Int64
open Array
open Color
open Command

exception End_loop

type board = color array array

let iinf = 1000000000000000
let inf  = 1000000000000

let int64_get board i j : bool =
  equal (logand (shift_right_logical board (i*8+j)) 0x1L) 0x1L

let int64_flip board i j : int64 =
  logxor board (shift_left 0x1L (i*8+j))

let int64_popcount x : int =
  let ( + ) = add in
  let ( * ) = mul in
  let ( lsr ) = shift_right_logical in
  let ( land ) = logand in
  let m1  = 0x5555555555555555L in
  let m2  = 0x3333333333333333L in
  let m4  = 0x0f0f0f0f0f0f0f0fL in
  let h01 = 0x0101010101010101L in
  let x = Int64.sub x (logand (shift_right_logical x 1) m1) in
  let x = (x land m2) + ((x lsr 2) land m2) in
  let x = (x + (x lsr 4)) land m4 in
  to_int ((x * h01) lsr 56)

let count board : int =
  int64_popcount !board

let convert_board board color : int64 ref =
  let ret = ref 0x0L in
  for i=1 to 8 do
    for j=1 to 8 do
      if board.(i).(j) = color then ret := int64_flip !ret (i-1) (j-1)
    done
  done;
  ret

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
  print_endline "  (X: Black,  O: White)"

let print_bit_board myboard opboard =
  print_endline " |A B C D E F G H ";
  print_endline "-+----------------";
  for j=0 to 7 do
    print_int j; print_string "|";
    for i=0 to 7 do
      if int64_get myboard j i then print_string "X"
      else if int64_get opboard j i then print_string "O"
      else print_string " ";
      print_string " "
    done;
    print_endline ""
  done;
  print_endline "  (X: Black,  O: White)"

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
    board

let dirs = [ (-1,-1); (0,-1); (1,-1); (-1,0); (1,0); (-1,1); (0,1); (1,1) ]

let rec get_list_max l i : (int * int) =
  match l with
  | []    -> (-100000000,-1)
  | s::ts -> let p = get_list_max ts (i+1) in if s > (fst p) then (s, i) else p

let flippable_indices_line myboard opboard (di,dj) (i,j) =
  let rec f (di,dj) (i,j) r =
    if i > 7 || i < 0 || j > 7 || j < 0 then
      []
    else if (int64_get !opboard i j) then
      g (di,dj) (i+di,j+dj) ( (i,j) :: r )
    else
      []
  and    g (di,dj) (i,j) r =
    if i > 7 || i < 0 || j > 7 || j < 0 then
      []
    else if (int64_get !opboard i j) then
      g (di,dj) (i+di,j+dj) ( (i,j) :: r )
    else if (int64_get !myboard i j) then
      r
    else
      [] in
    f (di,dj) (i,j) []

let flippable_indices myboard opboard (i,j) =
  let bs = List.map (fun (di,dj) -> flippable_indices_line myboard opboard (di,dj) (i+di,j+dj)) dirs in
    List.concat bs

let flip_count myboard opboard (i,j) : int =
  List.length (flippable_indices myboard opboard (i,j))

let is_valid_move myboard opboard (i,j) : bool =
  (int64_get (logor !myboard !opboard) i j = false) && (flip_count myboard opboard (i,j) > 0)

let empty_count myboard opboard =
  64 - (int64_popcount (logor !myboard !opboard))

let mix xs ys =
  List.concat (List.map (fun x -> List.map (fun y -> (x,y)) ys) xs)

let valid_moves myboard opboard =
  let ls = [0;1;2;3;4;5;6;7] in
  List.filter (is_valid_move myboard opboard)
    (mix ls ls)

let cell_value_list = [| [| 30;-12;  0; -1; -1;  0;-12; 30 |];
                         [|-12;-15; -3; -3; -3; -3;-15;-12 |];
                         [|  0; -3;  0; -1; -1;  0; -3;  0 |];
                         [| -1; -3; -1; -1; -1; -1; -3; -1 |];
                         [| -1; -3; -1; -1; -1; -1; -3; -1 |];
                         [|  0; -3;  0; -1; -1;  0; -3;  0 |];
                         [|-12;-15; -3; -3; -3; -3;-15;-12 |];
                         [| 30;-12;  0; -1; -1;  0;-12; 30 |] |]

let last_eval_board myboard opboard : int =
  (int64_popcount !myboard) - (int64_popcount !opboard)

let sub_fast_search myboard opboard ms =
  List.map (fun (i,j) ->
    let flip_cells = flippable_indices myboard opboard (i,j) in
    myboard := int64_flip !myboard i j;
    List.iter (fun (ii,jj) -> (myboard := int64_flip !myboard ii jj; opboard := int64_flip !opboard ii jj)) flip_cells;
    let ret = List.length (valid_moves opboard myboard) in
    List.iter (fun (ii,jj) -> (myboard := int64_flip !myboard ii jj; opboard := int64_flip !opboard ii jj)) flip_cells;
    myboard := int64_flip !myboard i j;
    (ret, (i,j))
  ) ms

let rec last_update_board myboard opboard depth best (i,j) : unit =
  let emp = empty_count myboard opboard in
  let flip_cells = flippable_indices myboard opboard (i,j) in
  if emp = 1 then
    let ret : int = (last_eval_board myboard opboard) + 1 + (List.length flip_cells) * 2 in
    if (fst !best) < ret then
      best := (ret, (i,j))
    else ();
    if ret > 0 then raise End_loop else ()
  else
    myboard := int64_flip !myboard i j;
    List.iter (fun (ii,jj) -> myboard := int64_flip !myboard ii jj; opboard := int64_flip !opboard ii jj) flip_cells;
    let ret : int = if depth > 0 then -1*(fst (last_deep_search opboard myboard (depth-1)))
                    else last_eval_board myboard opboard in
    List.iter (fun (ii,jj) -> myboard := int64_flip !myboard ii jj; opboard := int64_flip !opboard ii jj) flip_cells;
    myboard := int64_flip !myboard i j;
    if (fst !best) < ret then
      best := (ret, (i,j))
    else ();
    if ret > 0 then raise End_loop else ()

 and last_deep_search myboard opboard depth : (int * (int * int)) =
  let ms = valid_moves myboard opboard in
  let emp = empty_count myboard opboard in
  if emp = 0 then
    (last_eval_board myboard opboard, (-1, -1))
  else if List.length ms = 0 then
    if count myboard = 0 then (-inf, (-1, -1))
    else ((if depth > 0 then -1*(fst (last_deep_search opboard myboard (depth-1))) else last_eval_board myboard opboard), (-1,-1))
  else if emp > 4 then
    let fast_ms = List.map (fun x -> (snd x)) (List.sort (fun x y -> (fst x) - (fst y)) (sub_fast_search myboard opboard ms)) in
    let best = ref (-iinf, (-1,-1)) in
    (try
      List.iter (last_update_board myboard opboard depth best) fast_ms
    with End_loop -> ());
    !best
  else
    let best = ref (-iinf, (-1,-1)) in
    (try
      List.iter (last_update_board myboard opboard depth best) ms
    with End_loop -> ());
    !best

let eval_board myboard opboard : int =
  let value = ref 0 in
  let ms = valid_moves opboard myboard in
  for i=0 to 7 do
    for j=0 to 7 do
      value := !value + (if (int64_get !myboard i j) then cell_value_list.(i).(j) else if (int64_get !opboard i j) then -1*cell_value_list.(i).(j) else 0)
    done
  done; !value - (List.length ms) * 5

let rec update_board myboard opboard depth prebest best (i,j) : unit =
  let flip_cells = flippable_indices myboard opboard (i,j) in
  myboard := int64_flip !myboard i j;
  List.iter (fun (ii,jj) -> myboard := int64_flip !myboard ii jj; opboard := int64_flip !opboard ii jj) flip_cells;
  (* if depth > 1 then print_bit_board !myboard !opboard; *)
  let ret : int = if depth > 0 then -1*(fst (deep_search opboard myboard (fst !best) (depth-1)))
                  else eval_board myboard opboard in
  List.iter (fun (ii,jj) -> myboard := int64_flip !myboard ii jj; opboard := int64_flip !opboard ii jj) flip_cells;
  myboard := int64_flip !myboard i j;
  if (fst !best) < ret then
    best := (ret, (i,j))
  else ();
  if prebest >= -1*ret then
    raise End_loop
  else ();
  ()

 and deep_search myboard opboard prebest depth : (int * (int * int)) =
  let ms = valid_moves myboard opboard in
  if List.length ms = 0 then
    if count myboard = 0 then (-inf, (-1,-1))
    else ((if depth > 0 then -1*(fst (deep_search opboard myboard iinf (depth-1))) else eval_board myboard opboard), (-1,-1))
  else
    let best = ref (-iinf, (-1,-1)) in
    (try
      List.iter (update_board myboard opboard depth prebest best) ms;
    with End_loop -> ());
    !best

let play board color =
  print_board board;
  let ocolor  = opposite_color color in
  let myboard = convert_board board color in
  let opboard = convert_board board ocolor in
  print_bit_board !myboard !opboard;
  let ms = valid_moves myboard opboard in
    if ms = [] then
      Pass
    else
        (*
      if (empty_count myboard opboard >= 52) then
        let k = Random.int (List.length ms) in
        let (i,j) = List.nth ms k in
        Mv ((i+1),(j+1))
      else *) if (empty_count myboard opboard <= 12) then
        let best = last_deep_search myboard opboard 30 in
        Mv (((fst (snd best))+1), ((snd (snd best))+1))
      else
        let best = deep_search myboard opboard (-1*iinf) 3 in
        Mv (((fst (snd best))+1), ((snd (snd best))+1))

let old_count board color : int =
  let s = ref 0 in
    for i=1 to 8 do
      for j=1 to 8 do
        if board.(i).(j) = color then s := !s + 1
      done
    done;
    !s

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
    f (di,dj) (i,j) []

let old_flippable_indices board color (i,j) =
  let bs = List.map (fun (di,dj) -> old_flippable_indices_line board color (di,dj) (i+di,j+dj)) dirs in
    List.concat bs

let doMove board com color =
  match com with
      GiveUp  -> board
    | Pass    -> board
    | Mv (i,j) ->
	let ms = old_flippable_indices board color (i,j) in
	let _  = List.map (fun (ii,jj) -> board.(ii).(jj) <- color) ms in
	let _  = board.(i).(j) <- color in
	  board
    | _ -> board

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
    print_board board

