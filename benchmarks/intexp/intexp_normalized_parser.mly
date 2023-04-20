/*
 * Copyright (c) 2018 Neelakantan Krishnaswami and Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 */

%{
%}

/* Tokens */

%token LET IN IF THEN ELSE LPAREN RPAREN PLUS MINUS TIMES EQUAL
%token <int> INT
%token <string> ID
%token EOF

%start start
%type <(string * int) list -> int> start
%%

start: exp EOF                              { $1 }
;

else_: ELSE                                 { () }
then_: THEN                                 { () }
in_: IN                                     { () }
equal_: EQUAL                               { () }
id: ID                                      { $1 }
rparen: RPAREN                              { () }

atom: ID                                    { List.assoc $1 }
| INT                                       { fun _ -> $1 }
| LPAREN exp rparen                         { $2 }
;
timeses: TIMES atom timeses                 { fun env x -> $3 env (x * $2 env) }
| /* ε */                                   { fun _env x -> x }
;
timesexp: ID timeses                        { fun env -> $2 env (List.assoc $1 env) }
| INT timeses                               { fun env -> $2 env $1 }
| LPAREN exp rparen timeses                 { fun env -> $4 env ($2 env) }
;
pluses: PLUS timesexp pluses                { fun env x -> $3 env (x + $2 env) }
| MINUS timesexp pluses                     { fun env x -> $3 env (x - $2 env) }
| /* ε */                                   { fun _env x -> x }
;
times_plus: ID timeses pluses               { fun env -> $3 env ($2 env (List.assoc $1 env)) }
| INT timeses pluses                        { fun env -> $3 env ($2 env $1) }
| LPAREN exp rparen timeses pluses          { fun env -> $5 env ($4 env ($2 env)) }
;
equalses: EQUAL times_plus equalses         { fun env x -> $3 env (if (x = $2 env) then 1 else 0) }
| /* ε */                                   { fun _env x -> x }
;
exp:
| LET id equal_ exp in_ exp                 { fun env -> $6 (($2,$4 env) :: env) }
| ID timeses pluses equalses                { fun env -> $4 env ($3 env ($2 env (List.assoc $1 env))) }
| INT timeses pluses equalses               { fun env -> $4 env ($3 env ($2 env $1)) }
| IF exp then_ exp else_ exp                { fun env -> if $2 env <> 0 then $4 env else $6 env }
| LPAREN exp rparen timeses pluses equalses { fun env -> $6 env ($5 env ($4 env ($2 env))) }
;

%%

(* DGNF grammar produced by flap:

<exp,
{else_ ↦ ELSE,
then_ ↦ THEN,
in_ ↦ IN,
equal_ ↦ EQUAL,
id ↦ ID,
rparen ↦ RPAREN,
atom ↦ ID
         | INT 
         | LPAREN exp rparen,
timeses ↦ TIMES atom timeses
         | ε,
timesexp ↦ ID timeses  
         | INT timeses  
         | LPAREN exp rparen timeses,
pluses ↦ PLUS timesexp pluses  
         | MINUS timesexp pluses
         | ε,
times_plus ↦ ID timeses pluses  
         | INT timeses pluses  
         | LPAREN exp rparen timeses pluses,
equalses ↦ EQUAL times_plus equalses
         | ε,
exp:
         | LET id equal_ exp in_ exp  
         | ID timeses pluses equalses  
         | INT timeses pluses equalses  
         | IF exp then_ exp else_ exp  
         | LPAREN exp rparen timeses pluses equalses,
}>
*)
