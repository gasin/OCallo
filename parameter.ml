open Const

let rec read_param chan i num posi counter : unit =
  let a = input_char chan in
  let n = (Char.code a)-48 in
  let signed_num = (if posi then num else (-1*num)) in
  if a = '-' then
    read_param chan i iinf false counter
  else if (0 <= n && n <= 9) then
    if num = iinf then
      read_param chan i n posi counter
    else
      read_param chan i (num*10+n) posi counter
  else
    if num = iinf then
      read_param chan i iinf true counter
    else
      (
      (if counter = 0 then
        mountain_weight.(i) <- signed_num
      else if counter = 1 then
        solid_weight.(i) <- signed_num
      else if counter = 2 then
        next_put_weight.(i) <- signed_num
      else if counter = 3 then
        next_put_corner_weight.(i) <- signed_num
      else
        corner_weight.(i).((counter-4) / 16).((counter-4) mod 16) <- signed_num);
      read_param chan i iinf true (counter+1))


let loading_parameters () : unit =
  for i=0 to 6 do
    let chan = open_in ("./data/stage" ^ (string_of_int i) ^ ".txt") in
    (try read_param chan i iinf true 0 with End_of_file -> ();
    close_in chan)
  done
