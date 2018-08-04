open Hashtbl;;

let joseki_table : (string, int) Hashtbl.t = Hashtbl.create 10000000;;

let rotation = ref 0;;
let pre_board = ref 0x0L;;
let hand_history = ref "";;

let rec read_file chan history : unit =
  let a = input_char chan in
  if a = '\n' then
    read_file chan ""
  else
    let num = (Char.code a)-33 in
      (Hashtbl.add joseki_table history num;
      read_file chan (history ^ (Char.escaped a)))

let make_opening_book () : unit =
  let chan = open_in "./joseki/xxx_comp.gam" in
  (try read_file chan "" with End_of_file ->
  close_in chan)
