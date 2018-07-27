open Color

type wl   = Win | Lose | Tie 
type move = Mv of int * int | Pass | GiveUp

let string_of_move = function 
  | Pass   -> "PASS"
  | GiveUp -> "GIVEUP" 
  | Mv (i,j) -> 
    let ci = char_of_int (i + int_of_char 'A' - 1) in 
    let cj = char_of_int (j + int_of_char '1' - 1) in 
    let s  = String.make 2 ' ' in 
    let _  = ( s.[0] <- ci; s.[1] <- cj) in 
    s
  

type command = 
  | Open of string (* player name *) 
  | End of wl * int * int * string 
      (* result, your stones, opponent's stones, reason *) 
  | Move of move 
  | Start of color * string * int 
      (* color, oppnent's name, assinged time (in ms) *) 
  | Ack of int (* updated assigned time (in ms) *) 
  | Bye of (string * (int * int * int)) list 
  | Empty 

