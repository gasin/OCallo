open Int64;;
open Array;;
open Hashtbl;;

exception End_loop;;

let dirs = [ (-1,-1); (0,-1); (1,-1); (-1,0); (1,0); (-1,1); (0,1); (1,1) ];;
(*
let dirs_array = [| [(1,0);(-1,0)]; [(0,1);(0,-1)]; [(1,1);(-1,-1)]; [(1,-1);(-1,1)] |];;
*)

(*let hash_table = Hashtbl.create 100000;;*)
let last_hash_table : ((int64 * int64), (int*(int*int))) Hashtbl.t = Hashtbl.create 1000000;;
let empty_cells : ((int * int), unit) Hashtbl.t = Hashtbl.create 30;;

(*
let flip_table = Hashtbl.create 10000000;;
*)

let search_depth = 6
let last_search_depth = 21

let iinf = 1000000000000000;;
let inf  = 1000000000000;;

let corners = [(0,0); (0,7); (7,0); (7,7)];;
let corners_dir = [[(1,0); (0,1)]; [(1,0);(0,-1)]; [(-1,0);(0,1)]; [(-1,0);(0,-1)]];;

let edge_mountain = [| 0x7e00000000000000L;   0x80808080808000L;   0x1010101010100L; 0x7eL |];;
let edge_mountain_sub = [| 0x8100000000000000L; 0x8000000000000080L; 0x100000000000001L; 0x81L |];;

let cell_value_list = [| 100;-10;  0; -1; -1;  0;-10;100;
                         -10;-15; -3; -3; -3; -3;-15;-10;
                           0; -3;  0; -1; -1;  0; -3;  0;
                          -1; -3; -1; -1; -1; -1; -3; -1;
                          -1; -3; -1; -1; -1; -1; -3; -1;
                           0; -3;  0; -1; -1;  0; -3;  0;
                         -10;-15; -3; -3; -3; -3;-15;-10;
                         100;-10;  0; -1; -1;  0;-10;100 |];;
