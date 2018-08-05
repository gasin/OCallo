open Printf;;
open Hashtbl;;
open Utils;;
open Int64;;
open Search;;
open Const;;
open Parameter;;

open Joseki

let simulation_score = ref 0;;
let simulation_counter = ref 0;;

exception End_this;;

let stage = [| [| 13; 20 |]; [| 20; 25 |]; [| 25; 30 |]; [| 30; 35 |]; [| 35; 40 |]; [| 40; 45 |]; [| 45; 52 |] |];;
let min_stone = ref 0;;
let max_stone = ref 0;;
let best      = ref 0;;

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
      flush_all ()
      )
  else ();
  let now_best = deep_search myboard opboard search_depth in
  let (my_x,my_y) = snd now_best in
  if my_x*8 + my_y = ans then simulation_score := !simulation_score + 1
  else ())

(*
let mountain_weight_cand = [50; 40; 30; 20; 10]
let solid_weight_cand = [50; 40; 30; 20; 10]
let next_put_weight_cand = [ 20; 15; 10; 5; 0] 
let next_put_corner_weight_cand = [20; 15; 10; 5; 0]

let mwc_i = ref 2;;
let swc_i = ref 2;;
let npwc_i = ref 2;;
let npcwc_i = ref 2;;
let best = ref 2;;

let iterate_para i para num chan =
  let best_j = ref 2 in
  (for j = 0 to (num-1) do
    if j <> 2 then (
    simulation_score := 0;
    simulation_counter := 0;
    para := j;
    mountain_weight.(i) <- List.nth mountain_weight_cand !mwc_i;
    solid_weight.(i) <- List.nth solid_weight_cand !swc_i;
    next_put_weight.(i) <- List.nth next_put_weight_cand !npwc_i;
    next_put_corner_weight.(i) <- List.nth next_put_corner_weight_cand !npcwc_i;
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
*)

let rec corner_para_tune diff i j k chan =
  (
  simulation_score := 0;
  simulation_counter := 0;
  corner_weight.(i).(j).(k) <- corner_weight.(i).(j).(k) + diff;
  Hashtbl.iter translate joseki_table;
  if !simulation_score <= !best then
    corner_weight.(i).(j).(k) <- corner_weight.(i).(j).(k) - diff
  else
    (best := !simulation_score;
    (*
    output_string chan ((string_of_int !simulation_score) ^ " / " ^ (string_of_int !simulation_counter) ^ "\n");
    *)

    print_string ((string_of_int !simulation_score) ^ " / " ^ (string_of_int !simulation_counter) ^ "\n");
    for jj=0 to 15 do
      (
      for kk=0 to 15 do
          (*
        output_string chan ((string_of_int corner_weight.(i).(jj).(kk)) ^ " ")
        *)
        print_string ((string_of_int corner_weight.(i).(jj).(kk)) ^ " ")
      done;
      (*
      output_string chan "\n"
      *)
      print_string "\n"
      )
    done;
    (*
    output_string chan "\n";
    *)
    print_string "\n";
    corner_para_tune diff i j k chan)
  )

let main () =
  loading_parameters ();
  loading_book ();
  print_string "loaded\n";
  flush_all ();
  (*
  for i = 0 to 6 do
    let chan = open_out ("./data/stage" ^ (string_of_int i) ^ "-result.txt") in (
    min_stone := stage.(i).(0);
    max_stone := stage.(i).(1);
    mwc_i := 2; swc_i := 2; npwc_i := 2; npcwc_i := 2;
    mountain_weight.(i) <- List.nth mountain_weight_cand !mwc_i;
    solid_weight.(i) <- List.nth solid_weight_cand !swc_i;
    next_put_weight.(i) <- List.nth next_put_weight_cand !npwc_i;
    next_put_corner_weight.(i) <- List.nth next_put_corner_weight_cand !npcwc_i;

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

    iterate_para i mwc_i 5 chan;
    iterate_para i swc_i 5 chan;
    iterate_para i npwc_i 5 chan;
    iterate_para i npcwc_i 5 chan;
    close_out chan)
  done;;
  *)
  for i = 0 to 6 do
    let chan = open_out ("./data/stage" ^ (string_of_int i) ^ "-corner_result.txt") in (
    min_stone := stage.(i).(0);
    max_stone := stage.(i).(1);

    simulation_score := 0;
    simulation_counter := 0;
    Hashtbl.iter translate joseki_table;
    best := !simulation_score;
    (*
    output_string chan ((string_of_int !simulation_score) ^ " / " ^ (string_of_int !simulation_counter) ^ "\n");
    *)
    print_string ((string_of_int !simulation_score) ^ " / " ^ (string_of_int !simulation_counter) ^ "\n");
    for jj=0 to 15 do
      (
      for kk=0 to 15 do
          (*
        output_string chan ((string_of_int corner_weight.(i).(jj).(kk)) ^ " ")
        *)
        print_string ((string_of_int corner_weight.(i).(jj).(kk)) ^ " ")
      done;
      (*
      output_string chan "\n"
      *)
      print_string "\n"
      )
    done;
    (*
    output_string chan "\n";
    *)
    print_string "\n";
    close_out chan

(*
    for j=0 to 15 do
      for k=0 to 15 do
        (print_int j; print_string " "; print_int k; print_newline ();
        if (j land k) = 0 then
          (corner_para_tune 20 i j k chan;
          corner_para_tune (-1*20) i j k chan)
        else ())
      done
    done;
    close_out chan
*)
    )
 done;;


main ()
