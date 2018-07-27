%token <int> INT
%token OPEN END MOVE START ACK BYE 
%token WIN LOSE TIE
%token WHITE BLACK
%token NL EOF 
%token <string> STR 
%token SPACE

%start comm
%type <Command.command> comm

%%

comm:
| OPEN STR eoc { Command.Open $2 }
| END wl INT INT STR eoc { Command.End ($2,$3,$4,$5) }
| MOVE STR eoc { let s = $2 in
             if s = "PASS" then Command.Move Command.Pass
             else if s = "GIVEUP" then Command.Move Command.GiveUp
             else if String.length s = 2 && 'A' <= s.[0] && s.[0] <= 'H' 
                  && '1' <= s.[1] && s.[1] <= '8' then 
               Command.Move 
                 (Command.Mv (int_of_char s.[0] - int_of_char 'A' + 1, 
                              int_of_char s.[1] - int_of_char '1' + 1))
             else
               failwith "Invalid Command" }
| START wb STR INT eoc  { Command.Start ($2,$3,$4) }
| ACK   INT eoc         { Command.Ack   $2 }
| BYE   scores eoc      { Command.Bye   $2 }
| eoc                   { Command.Empty }
;
               
wl:          
| WIN  { Command.Win }
| LOSE { Command.Lose }
| TIE  { Command.Tie }                
;

wb:
| WHITE { Color.white }
| BLACK { Color.black }
;

scores:
| STR INT INT INT        { [($1,($2,$3,$4))] }
| STR INT INT INT scores { ($1,($2,$3,$4)) :: $5 }
;

eoc:
  | NL  {}
  | EOF {}
;
