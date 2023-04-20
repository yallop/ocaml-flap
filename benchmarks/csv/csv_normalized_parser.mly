/*
 * Copyright (c) 2021 Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 */

%{
%}

/* Tokens */

%token CRLF COMMA
%token <string> FIELD
%token EOF

%start file
%type <int> file recordPlus
%type <int option> recordStar
%%

rest: /* */ { 0 }
| COMMA field rest { 1 + $3 }

field: FIELD { () }

crlf: CRLF { () }

recordPlus:
 FIELD rest crlf recordStar { match $4 with None -> (1+$2) | Some r -> ((assert ((1+$2) = r); r)) }

recordStar:
| FIELD rest crlf recordStar { match $4 with None -> Some (1+$2) | Some r -> ((assert ((1+$2) = r); Some r)) }
| /* empty */  { None }

file: recordPlus EOF { $1 }

%%

(* DGNF grammar produced by flap:

<file,
{recordStar ↦ FIELD rest crlf recordStar | ε,
crlf ↦ CRLF,
field ↦ FIELD,
rest ↦ COMMA field rest | ε,
file ↦ FIELD rest crlf recordStar, }>
 *)
