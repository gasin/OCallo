open Array 
open Color 
open Command 


type board = color array array 

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
    
let is_effective board color (i,j) =
  match flippable_indices board color (i,j) with 
      [] -> false
    | _  -> true 

let is_valid_move board color (i,j) =
  (board.(i).(j) = none) && is_effective board color (i,j) 


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


let play board color = 
  let ms = valid_moves board color in 
    if ms = [] then 
      Pass 
    else 
      let k = Random.int (List.length ms) in 
      let (i,j) = List.nth ms k in 
	Mv (i,j) 

let count board color = 
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

