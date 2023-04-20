/*
 * Copyright (c) 2018 Neelakantan Krishnaswami and Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 */

%{
%}

/* Tokens */

%token LBRACKET RBRACKET LBRACE RBRACE COMMA COLON
%token NULL TRUE FALSE
%token <string> STRING DECIMAL
%token EOF

%start start
%type <int> start
%%

start: valueStar EOF                               { $1 }

valueStar: /**/                                    { 0 }
| LBRACE member_list rbrace                   valueStar { $2 + $4 }
| LBRACKET value_list rbracket                valueStar { $2 + $4 }
| STRING                                      valueStar { 1 + $2}
| DECIMAL                                     valueStar { 1 + $2 }
| NULL                                        valueStar { 1 + $2 }
| TRUE                                        valueStar { 1 + $2 }
| FALSE                                       valueStar { 1 + $2 }
;

value: LBRACE member_list rbrace                   { $2 }
|      LBRACKET value_list rbracket                { $2 }
|      STRING                                      { 1 }
|      DECIMAL                                     { 1 }
|      NULL                                        { 1 }
|      TRUE                                        { 1 }
|      FALSE                                       { 1 }
;

member_list: /* */                                 { 0 }
| STRING valopt member_list_rest_opt               { 1 + $2 + $3 }
;

member_list_rest_opt: /* */                        { 0 }
| COMMA member_list                                { $2 }
;

value_list: /* */                                  { 0 }
| LBRACE member_list rbrace value_list_rest_opt    { $2 + $4 }
| LBRACKET value_list rbracket value_list_rest_opt { $2 + $4}
| STRING value_list_rest_opt                       { 1 + $2 }
| DECIMAL value_list_rest_opt                      { 1 + $2 }
| NULL value_list_rest_opt                         { 1 + $2 }
| TRUE value_list_rest_opt                         { 1 + $2 }
| FALSE value_list_rest_opt                        { 1 + $2 }
;

value_list_rest_opt: /* */                         { 0 }
| COMMA value_list                                 { $2 }
;

valopt: COLON value                                { $2 }
|        /* */                                     { 0  }

rbrace: RBRACE                                     { () }
rbracket: RBRACKET                                 { () }
%%

(* DGNF grammar produced by flap:

<values,
{rbracket ↦ RBRACKET,
value_list_rest ↦ COMMA value_list
                 | ε,
value_list ↦ STRING value_list_rest  
         | LBRACE member_list rbrace value_list_rest  
         | LBRACKET value_list rbracket value_list_rest  
         | DECIMAL value_list_rest  
         | NULL value_list_rest  
         | TRUE value_list_rest  
         | FALSE value_list_rest
         | ε,
rbrace ↦ RBRACE,
member_list_rest_opt ↦ COMMA member_list
                      | ε,
valopt ↦ COLON value
         | ε,
member_list ↦ STRING valopt member_list_rest_opt
             | ε,
value ↦ STRING  
       | LBRACE member_list rbrace  
       | LBRACKET value_list rbracket  
       | DECIMAL
       | NULL 
       | TRUE
       | FALSE,
values ↦ STRING values  
        | LBRACE member_list rbrace values  
        | LBRACKET value_list rbracket values  
        | DECIMAL values  
        | NULL values  
        | TRUE values  
        | FALSE values
        | ε,
}>
 *)
