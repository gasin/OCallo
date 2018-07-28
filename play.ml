open Array 
open Color 
open Command 

exception End_loop

type board = color array array 

let iinf = 1000000000000000
let inf  = 1000000000000

let count board color : int = 
  let s = ref 0 in 
    for i=1 to 8 do 
      for j=1 to 8 do
        if board.(i).(j) = color then s := !s + 1 
      done
    done;
    !s

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

let flippable_indices_line board color (di,dj) (i,j) =
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

let flippable_indices board color (i,j) =
  let bs = List.map (fun (di,dj) -> flippable_indices_line board color (di,dj) (i+di,j+dj)) dirs in 
    List.concat bs 
    
let flip_count board color (i,j) =
  List.length (flippable_indices board color (i,j))

let is_valid_move board color (i,j) =
  (board.(i).(j) = none) && (flip_count board color (i,j) > 0)

let empty_count board =
  let r = ref 0 in
  for i=1 to 8 do
    for j=1 to 8 do
      r := !r + if board.(i).(j) = none then 1 else 0
    done
  done; !r

let doMove board com color =
  match com with 
      GiveUp  -> board
    | Pass    -> board
    | Mv (i,j) -> 
	let ms = flippable_indices board color (i,j) in 
	let _  = List.map (fun (ii,jj) -> board.(ii).(jj) <- color) ms in 
	let _  = board.(i).(j) <- color in 
	  board 
    | _ -> board 

let mix xs ys =
  List.concat (List.map (fun x -> List.map (fun y -> (x,y)) ys) xs)

let valid_moves board color = 
  let ls = [1;2;3;4;5;6;7;8] in 
  List.filter (is_valid_move board color)
    (mix ls ls)

let cell_value_list = [| [| 30; -12; 0; -1; -1; 0; -12; 30 |];
                         [| -12; -15; -3; -3; -3; -3; -15; -12 |];
                         [| 0; -3; 0; -1; -1; 0; -3; 0 |];
                         [| -1; -3; -1; -1; -1; -1; -3; -1 |];
                         [| -1; -3; -1; -1; -1; -1; -3; -1 |];
                         [| 0; -3; 0; -1; -1; 0; -3; 0 |];
                         [| -12; -15; -3; -3; -3; -3; -15; -12 |];
                         [| 30; -12; 0; -1; -1; 0; -12; 30 |] |]

let last_eval_board board color : int =
  let ocolor = opposite_color color in
  (count board color) - (count board ocolor)

let rec last_update_board board color depth best (i,j) : unit = 
  let ocolor = opposite_color color in
  let flip_cells = flippable_indices board color (i,j) in
  board.(i).(j) <- color;
  List.iter (fun (ii,jj) -> (board.(ii).(jj) <- color);) flip_cells; 
  let ret : int = if depth > 0 then -1*(fst (last_deep_search board ocolor (depth-1))) 
                  else last_eval_board board color in
  List.iter (fun (ii,jj) -> (board.(ii).(jj) <- ocolor);) flip_cells; 
  board.(i).(j) <- none;
  if (fst !best) < ret then  
    best := (ret, (i,j))
  else ();
  if ret > 0 then raise End_loop else ()

 and last_deep_search board color depth : (int * (int * int)) =
  let ocolor = opposite_color color in
  let ms = valid_moves board color in 
  if empty_count board = 0 then
    (last_eval_board board color, (-1, -1))
  else if List.length ms = 0 then
    if count board color = 0 then (-inf, (-1, -1))
    else ((if depth > 0 then -1*(fst (last_deep_search board ocolor (depth-1))) else last_eval_board board color), (-1,-1))
  else
    let best = ref (-iinf, (-1,-1)) in 
    (try
      List.iter (last_update_board board color depth best) ms
    with End_loop -> ());
    !best

let eval_board board color : int =
  let value = ref 0 in 
  let ocolor = opposite_color color in
  for i=1 to 8 do
    for j=1 to 8 do
      value := !value + (if board.(i).(j) = color then cell_value_list.(i-1).(j-1) else if board.(i).(j) = ocolor then -1*cell_value_list.(i-1).(j-1) else 0)
    done
  done; !value

let rec update_board board color depth prebest best (i,j) : unit =
  let ocolor = opposite_color color in
  let flip_cells = flippable_indices board color (i,j) in
  board.(i).(j) <- color;
  List.iter (fun (ii,jj) -> (board.(ii).(jj) <- color);) flip_cells; 
  let ret : int = if depth > 0 then -1*(fst (deep_search board ocolor (fst !best) (depth-1))) 
                  else eval_board board color in
  List.iter (fun (ii,jj) -> (board.(ii).(jj) <- ocolor);) flip_cells; 
  board.(i).(j) <- none;
  if (fst !best) < ret then  
    best := (ret, (i,j))
  else ();
  if prebest <= -1*ret then
    raise End_loop
  else ();
  ()

 and deep_search board color prebest depth : (int * (int * int)) =
  let ocolor = opposite_color color in
  let ms = valid_moves board color in 
  if List.length ms = 0 then
    if count board color = 0 then (-inf, (-1,-1))
    else ((if depth > 0 then -1*(fst (deep_search board ocolor iinf (depth-1))) else eval_board board color), (-1,-1))
  else
    let best = ref (-iinf, (-1,-1)) in 
    (try
      List.iter (update_board board color depth prebest best) ms;
    with End_loop -> ());
    !best

let play board color = 
  print_board board;
  let ms = valid_moves board color in 
    if ms = [] then 
      Pass 
    else 
      if (empty_count board >= 52) then
        let k = Random.int (List.length ms) in 
        let (i,j) = List.nth ms k in 
        Mv (i,j)
      else if (empty_count board <= 14) then
        let best = last_deep_search board color 30 in
        Mv ((fst (snd best)), (snd (snd best)))
      else 
        let best = deep_search board color iinf 4 in
        Mv ((fst (snd best)), (snd (snd best)))

let report_result board = 
  let _ = print_endline "========== Final Result ==========" in 
  let bc = count board black in 
  let wc = count board white in 
    if bc > wc then 
      print_endline "*Black wins!*" 
    else if bc < wc then 
      print_endline "*White wins!*" 
    else
      print_endline "*Even*";
    print_string "Black: "; print_endline (string_of_int bc);
    print_string "White: "; print_endline (string_of_int wc);
    print_board board 

