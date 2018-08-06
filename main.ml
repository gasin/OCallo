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
  if !simulation_counter mod 1000 = 0 then
      (print_int !simulation_score;
      print_string " / ";
      print_int !simulation_counter;
      print_string "\n";
      flush_all ();
      raise End_this
      )
  else ();
  let now_best = deep_search myboard opboard search_depth in
  let (my_x,my_y) = snd now_best in
  if my_x*8 + my_y = ans then simulation_score := !simulation_score + 1
  else ())

let best_mountain_weight = Array.make 7 0;;
let best_solid_weight = Array.make 7 0;;
let best_next_put_weight = Array.make 7 0;;
let best_next_put_corner_weight = Array.make 7 0;;
let best_corner_weight = Array.init 7 (fun _ -> Array.init 16 (fun _ -> Array.make 16 0));;
let best_halfedge_weight = Array.init 7 (fun _ -> Array.init 16 (fun _ -> Array.make 16 0));;

let init () =
  for i = 0 to 6 do (
    best_mountain_weight.(i) <- mountain_weight.(i);
    best_solid_weight.(i) <- solid_weight.(i);
    best_next_put_weight.(i) <- next_put_weight.(i);
    best_next_put_corner_weight.(i) <- next_put_corner_weight.(i);
    for j = 0 to 15 do
      for k = 0 to 15 do
        best_corner_weight.(i).(j).(k) <- corner_weight.(i).(j).(k)
      done
    done;
    for j = 0 to 15 do
      for k = 0 to 15 do
        best_halfedge_weight.(i).(j).(k) <- halfedge_weight.(i).(j).(k)
      done
    done
  )
  done

let main () = (
  loading_book ();
  loading_parameters ();
  print_string "loaded\n";
  flush_all ();
  init ();
  for i = 0 to 6 do (
    min_stone := stage.(i).(0);
    max_stone := stage.(i).(1);

    simulation_score := 0;
    simulation_counter := 0;
    try
    Hashtbl.iter translate joseki_table;
    with End_this -> ();
    best := !simulation_score;

    for u = 0 to 6 do (
      print_int i; print_string " "; print_int u; print_newline ();
      simulation_score := 0;
      simulation_counter := 0;
      mountain_weight.(i) <- best_mountain_weight.(i) + (Random.int 100) - 50;
      solid_weight.(i) <- best_mountain_weight.(i) + (Random.int 100) - 50;
      next_put_weight.(i) <- best_next_put_weight.(i) + (Random.int 100) - 50;
      next_put_corner_weight.(i) <- best_next_put_corner_weight.(i) + (Random.int 100) - 50;
      for j=0 to 15 do
        for k=0 to 15 do
          if (j land k) = 0  && (j <> k) then
            halfedge_weight.(i).(j).(k) <- best_halfedge_weight.(i).(j).(k) + (Random.int 100) - 50
          else
            halfedge_weight.(i).(j).(k) <- 0
        done
      done;
      for j=0 to 15 do
        for k=0 to j do
          (
          if (j land k) = 0 then (
            if (j = k) then
              corner_weight.(i).(j).(k) <- 0
            else if ((j land 6) = 4 || (k land 6) = 2) then
              ()
            else
              (
              corner_weight.(i).(j).(k) <- best_corner_weight.(i).(j).(k) + (Random.int 100) - 50;
              corner_weight.(i).(k).(j) <- -1 * corner_weight.(i).(j).(k);
              if (j land 6 = 2) then
                if (k land 6 = 4) then
                (corner_weight.(i).(j lxor 6).(k lxor 6) <- corner_weight.(i).(j).(k);
                corner_weight.(i).(k lxor 6).(j lxor 6) <- -1 * corner_weight.(i).(j).(k))
                else
                (corner_weight.(i).(j lxor 6).(k) <- corner_weight.(i).(j).(k);
                corner_weight.(i).(k).(j lxor 6) <- -1 * corner_weight.(i).(j).(k))
              else if (k land 6 = 4) then
                (corner_weight.(i).(j).(k lxor 6) <- corner_weight.(i).(j).(k);
                corner_weight.(i).(k lxor 6).(j) <- -1 * corner_weight.(i).(j).(k))
              )
            )
          else ())
        done
      done;
      try
      Hashtbl.iter translate joseki_table;
      with End_this -> ();
      if !simulation_score > !best then
        let chan = open_out_gen [Open_wronly; Open_append; Open_creat] 0o666 ("./data/new_stage" ^ (string_of_int i) ^ "-score.txt") in (
          output_string chan ((string_of_int !simulation_score) ^ " / " ^ (string_of_int !simulation_counter) ^ "\n");
          print_string ("updated : " ^ (string_of_int !simulation_score) ^ " / " ^ (string_of_int !simulation_counter) ^ "\n");
          close_out chan);
        let chan = open_out ("./data/stage" ^ (string_of_int i) ^ ".txt") in (
          output_string chan (string_of_int mountain_weight.(i) ^ " ");
          output_string chan (string_of_int solid_weight.(i) ^ " ");
          output_string chan (string_of_int next_put_weight.(i) ^ " ");
          output_string chan (string_of_int next_put_corner_weight.(i) ^ "\n");
          for jj=0 to 15 do
            (
              for kk=0 to 15 do
                output_string chan ((string_of_int corner_weight.(i).(jj).(kk)) ^ " ")
              done;
              output_string chan "\n"
            )
          done;
          for jj=0 to 15 do
            (
              for kk=0 to 15 do
                output_string chan ((string_of_int halfedge_weight.(i).(jj).(kk)) ^ " ")
              done;
              output_string chan "\n"
            )
          done;
          output_string chan "\n";
          close_out chan);
        best := !simulation_score;
        best_mountain_weight.(i) <- mountain_weight.(i);
        best_solid_weight.(i) <- solid_weight.(i);
        best_next_put_weight.(i) <- next_put_weight.(i);
        best_next_put_corner_weight.(i) <- next_put_corner_weight.(i);
        for jj=0 to 15 do
          for kk=0 to 15 do
            (best_corner_weight.(i).(jj).(kk) <- corner_weight.(i).(jj).(kk);
            best_halfedge_weight.(i).(jj).(kk) <- halfedge_weight.(i).(jj).(kk))
          done
        done
      else ();
    );
    done;
    )
 done);;


main ()
