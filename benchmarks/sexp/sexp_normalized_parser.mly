/*
 * Copyright (c) 2018 Neelakantan Krishnaswami and Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 */

%{
%}

/* Tokens */

%token LPAREN RPAREN
%token<string> ATOM
%token EOF

%start start
%type <int> start
%%

start:   sexp EOF                { $1 }
;

sexp:    ATOM                    { 1 }
|        LPAREN sexps rpar       { $2 }
;

rpar:    RPAREN                  { () }
;

sexps:   /* */                   { 0 }
|        ATOM sexps              { 1 + $2 }
|        LPAREN sexps rpar sexps { $2 + $4 }
;

%%

(* DGNF grammar produced by flap:

start: sexp

rpar ↦ RPAREN

sexps ↦ LPAREN sexps rpar sexps
       | ATOM sexps
       | ε,

sexp ↦ LPAREN sexps rpar
       | ATOM
   
*)
