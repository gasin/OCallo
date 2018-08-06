open Int64;;
open Array;;
open Hashtbl;;

exception End_loop;;

let random_range = 0
let search_depth = 1
let last_search_depth = 19

let stage_sepa = [| 20+1-search_depth; 25+1-search_depth; 30+1-search_depth;
                    35+1-search_depth; 40+1-search_depth; 45+1-search_depth |]

let mountain_weight = Array.make 7 0;;
let solid_weight = Array.make 7 0;;
let next_put_weight = Array.make 7 0;;
let next_put_corner_weight = Array.make 7 0;;
let corner_weight = Array.init 7 (fun _ -> Array.init 16 (fun _ -> Array.make 16 0));;

let iinf = 1000000000000000;;
let inf  = 1000000000000;;

let corners = [(0,0); (0,7); (7,0); (7,7)];;
let corners_dir = [[(1,0); (0,1)]; [(1,0);(0,-1)]; [(-1,0);(0,1)]; [(-1,0);(0,-1)]];;
let dirs = [ (-1,-1); (0,-1); (1,-1); (-1,0); (1,0); (-1,1); (0,1); (1,1) ];;
let around_corner = [| [|  0;  1;  8;  9 |];
                       [|  7;  6; 15; 14 |];
                       [| 56; 57; 48; 49 |];
                       [| 63; 62; 55; 54 |] |];;


(*let hash_table : ((int64 * int64), (int*int)) Hashtbl.t = Hashtbl.create 100000;; *)
let last_hash_table : ((int64 * int64), (int*(int*int))) Hashtbl.t = Hashtbl.create 1000000;;
let empty_cells : ((int * int), unit) Hashtbl.t = Hashtbl.create 30;;
(*let flip_table = Hashtbl.create 10000000;;*)

let edge_line         = [| 0xff00000000000000L; 0x8080808080808080L; 0x101010101010101L; 0xffL |];;
let edge_mountain     = [| 0x7e00000000000000L;   0x80808080808000L;   0x1010101010100L; 0x7eL |];;
let edge_mountain_sub = [| 0x8100000000000000L; 0x8000000000000080L; 0x100000000000001L; 0x81L |];;
let corner_mask       =    0xc0c0000000000000L;;

let cell_value_list = Array.make 64 0;
