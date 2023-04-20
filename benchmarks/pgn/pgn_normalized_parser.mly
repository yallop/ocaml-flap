/*
 * Copyright (c) 2018 Neelakantan Krishnaswami and Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 */

%{
%}

/* Tokens */

%token LBRACKET RBRACKET MINUS SLASH STAR CASTLE
%token <string> TAG STRING COORDINATE INT INTDOT
%token EOF

%start start
%type <[`DRAWN | `OTHER | `WON] list> start
%type <[`COORD of string | `CASTLE]> coordinate
%type <([`CASTLE | `COORD of string ] option)> coordinate_opt
%%

start: games EOF                          { $1 }
;

int: INT { $1 }
;
slash: SLASH { () }
;
minus: MINUS { () }
;
rbracket: RBRACKET { () }
;
string: STRING { $1 }
;
tag: TAG { $1 }
;
result: STAR {`OTHER}
 | INT result_rest { $2 }
;
moves: INTDOT coordinate coordinate_opt moves { ($2, $3) :: $4 }
 | /* ε */ { [] }
;
games_opt:  LBRACKET tag string rbracket metadata moves result games_opt { let _ = (($2,$3) :: $5) in $7 :: $8 }
       | /* ε */  { [] }
;
result_rest: MINUS int  { `WON }
       | SLASH int minus int slash int { `DRAWN }
;
coordinate_opt: COORDINATE   { Some (`COORD $1 ) }
      | CASTLE { Some `CASTLE }
      | /* */ { None }
;
coordinate: COORDINATE  { `COORD $1 }
       | CASTLE { `CASTLE }
;
metadata: LBRACKET tag string rbracket metadata { ($2,$3) :: $5 }
  | /* ε */ { [] }
;
games: LBRACKET tag string rbracket metadata moves result games_opt   { let _ = (($2,$3) :: $5) in $7 :: $8 }
;

%%


(* DGNF grammar produced by flap:
<games,
{int ↦ INT,
slash ↦ SLASH,
minus ↦ MINUS,
rbracket ↦ RBRACKET,
string ↦ STRING,
tag ↦ TAG,
result ↦ STAR  | INT result_rest,
coordinate ↦ COORDINATE  
       | CASTLE,
moves ↦ INTDOT coordinate coordinate_opt moves | ε,
games_opt ↦  LBRACKET tag string rbracket metadata moves result games_opt 
       | ε,
result_rest ↦ MINUS int  
       | SLASH int minus int slash int,
coordinate_opt ↦ COORDINATE  
      | CASTLE
      | ε,
metadata ↦ LBRACKET tag string rbracket metadata  | ε,
games ↦ LBRACKET tag string rbracket metadata moves result games_opt
}>
 *)
