/*
 * Copyright (c) 2018 Neelakantan Krishnaswami and Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 */

%{
%}

/* Tokens */

%token P3
%token <int> INT
%token EOF

%start image
%type <bool> image
%%

image: P3 int int int ints EOF { let w, h, max = $2, $3, $4
                                 and max', count = $5 in
                                 (max' <= max) && (count = 3 * w * h) }
;

int: INT { $1 }

ints: /* */                { (min_int, 0) }
|     INT ints             { (max $1 (fst $2), succ (snd $2)) }
;

%%
(* DGNF grammar produced by flap:
<image,
{ints ↦ INT ints | ε,
 int ↦  INT,
 image ↦ P3 int int int ints,}>
 *)
