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
%type <int> file
%%

rest: /* */ { 0 }
| COMMA FIELD rest { 1 + $3 }

record: FIELD rest CRLF { 1 + $2 }

records: record { $1 }
|        record records { let r = $2 in (assert ($1 = r); r) }

file: records EOF { $1 }

%%
