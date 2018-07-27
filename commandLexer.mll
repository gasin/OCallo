{
open CommandParser 
}

let digit = ['0'-'9']
let nsstr = [^ ' ' '\t' '\n' '\r']+

rule token = parse 
| [' ' '\t']* { token lexbuf }
| '\n'        { NL } 
| '-'?digit+ as n { INT (int_of_string n) }
| "OPEN"      { OPEN }
| "END"       { END  }
| "MOVE"      { MOVE }
| "START"     { START }
| "ACK"       { ACK }
| "BYE"       { BYE }
| "WIN"       { WIN }
| "LOSE"      { LOSE }
| "TIE"       { TIE }
| "WHITE"     { WHITE }
| "BLACK"     { BLACK }
| nsstr as s  { STR s }
| eof         { EOF } 
