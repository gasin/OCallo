open Printf;;
open Hashtbl;;
open Utils;;
open Int64;;
open Search;;
open Const;;

open Joseki

let simulation_score = ref 0;;
let simulation_counter = ref 0;;

exception End_this;;

let stage = [| [| 13; 20 |]; [| 20; 25 |]; [| 25; 30 |]; [| 30; 35 |]; [| 35; 40 |]; [| 40; 45 |]; [| 45; 52 |] |];;
let min_stone = ref 0;;
let max_stone = ref 0;;

let translate (myboard,opboard) ans : unit =
  let stones = int64_popcount (logor myboard opboard) in
  if stones < !min_stone || stones >= !max_stone then ()
  else
  (simulation_counter := !simulation_counter + 1;
  if !simulation_counter mod 100000 = 0 then
      (print_int !simulation_score;
      print_string " / ";
      print_int !simulation_counter;
      print_string "\n";
      flush_all ();
      )
  else ();
  let best = deep_search myboard opboard search_depth in
  let (my_x,my_y) = snd best in
  if my_x*8 + my_y = ans then simulation_score := !simulation_score + 1
  else ())

let mountain_weight_cand = [50; 40; 30; 20; 10]
let solid_weight_cand = [50; 40; 30; 20; 10]
let next_put_weight_cand = [ 20; 15; 10; 5; 0] 
let next_put_corner_weight_cand = [20; 15; 10; 5; 0]

let mwc_i = ref 2;;
let swc_i = ref 2;;
let npwc_i = ref 2;;
let npcwc_i = ref 2;;
let best = ref 2;;

let iterate_para para num chan =
  let best_j = ref 2 in
  (for j = 0 to (num-1) do
    if j <> 2 then (
    simulation_score := 0;
    simulation_counter := 0;
    para := j;
    mountain_weight := List.nth mountain_weight_cand !mwc_i;
    solid_weight := List.nth solid_weight_cand !swc_i;
    next_put_weight := List.nth next_put_weight_cand !npwc_i;
    next_put_corner_weight := List.nth next_put_corner_weight_cand !npcwc_i;
    Hashtbl.iter translate joseki_table;
    if !simulation_score > !best then
      (best_j := j;
      best := !simulation_score;
      output_string chan (
              (string_of_int !simulation_score) ^ " / " ^ (string_of_int !simulation_counter) ^ " " ^
              (string_of_int !mountain_weight) ^ " " ^
              (string_of_int !solid_weight) ^ " " ^
              (string_of_int !next_put_weight) ^ " " ^
              (string_of_int !next_put_corner_weight) ^ "\n"))
     else ())
     else ()
  done;
  para := !best_j)

let main () =
  loading_book ();
  print_string "loaded\n";
  flush_all ();
  for i = 0 to 6 do
    let chan = open_out ("./data/stage" ^ (string_of_int i) ^ "-result.txt") in (
    min_stone := stage.(i).(0);
    max_stone := stage.(i).(1);
    mwc_i := 2; swc_i := 2; npwc_i := 2; npcwc_i := 2;
    mountain_weight := List.nth mountain_weight_cand !mwc_i;
    solid_weight := List.nth solid_weight_cand !swc_i;
    next_put_weight := List.nth next_put_weight_cand !npwc_i;
    next_put_corner_weight := List.nth next_put_corner_weight_cand !npcwc_i;

    simulation_score := 0;
    simulation_counter := 0;
    Hashtbl.iter translate joseki_table;
    best := !simulation_score;
    output_string chan (
            (string_of_int !simulation_score) ^ " / " ^ (string_of_int !simulation_counter) ^ " " ^
            (string_of_int !mountain_weight) ^ " " ^
            (string_of_int !solid_weight) ^ " " ^
            (string_of_int !next_put_weight) ^ " " ^
            (string_of_int !next_put_corner_weight) ^ "\n");

    iterate_para mwc_i 5 chan;
    iterate_para swc_i 5 chan;
    iterate_para npwc_i 5 chan;
    iterate_para npcwc_i 5 chan;
    close_out chan)
  done;;

main ()
