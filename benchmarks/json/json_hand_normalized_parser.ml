(*
 * Copyright (c) 2023 Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

type t = COMMA | COLON | STRING of string | LBRACE | RBRACE
       | LBRACKET | RBRACKET | DECIMAL of string | NULL | TRUE | FALSE | EOF

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
module Unfused =
struct

  let rec rbracket toks =
    match Stream.peek toks with
    | Some RBRACKET -> Stream.junk toks; ()
    | _ -> failwith "parsing failure"

  and value_list_rest toks =
    match Stream.peek toks with
    | Some COMMA -> Stream.junk toks; value_list toks
    | _ -> (* ε *) 0

  and value_list toks =
    match Stream.peek toks with
    | Some (STRING _) -> Stream.junk toks; 1 + value_list_rest toks  
    | Some LBRACE -> Stream.junk toks; let v1 = member_list toks in let () = rbrace toks in let v2 = value_list_rest toks in v1 + v2
    | Some LBRACKET -> Stream.junk toks; let v1 = value_list toks in let () = rbracket toks in let v2 = value_list_rest toks in v1 + v2
    | Some (DECIMAL _) -> Stream.junk toks; 1 + value_list_rest toks
    | Some NULL -> Stream.junk toks; 1 + value_list_rest toks
    | Some TRUE -> Stream.junk toks; 1 + value_list_rest toks
    | Some FALSE -> Stream.junk toks; 1 + value_list_rest toks
    | _ -> (* ε *) 0

  and rbrace (toks : _ Stream.t) =
    match Stream.peek toks with
    | Some RBRACE -> Stream.junk toks; ()
    | _ -> failwith "parsing failure"

  and member_list_rest_opt toks =
    match Stream.peek toks with
    | Some COMMA -> Stream.junk toks; member_list toks
    | _ -> (* ε *) 0

  and valopt toks =
    match Stream.peek toks with
    | Some COLON -> Stream.junk toks; value toks
    | _ -> (* ε *) 0

  and member_list toks =
    match Stream.peek toks with
    | Some (STRING _) -> Stream.junk toks; let v1 = valopt toks in let v2 = member_list_rest_opt toks in 1 + v1 + v2
    | _ -> (* ε *) 0

  and value toks =
    match Stream.peek toks with
    | Some (STRING _) -> Stream.junk toks; 1
    | Some LBRACE -> Stream.junk toks; let v = member_list toks in let () = rbrace toks in v
    | Some LBRACKET -> Stream.junk toks; let v = value_list toks in let () = rbracket toks in v
    | Some (DECIMAL _) -> Stream.junk toks; 1
    | Some NULL -> Stream.junk toks; 1
    | Some TRUE -> Stream.junk toks; 1
    | Some FALSE -> Stream.junk toks; 1
    | _ -> failwith "parsing failure"

  and values toks =
    match Stream.peek toks with
  | Some (STRING _) -> Stream.junk toks; 1 + values toks
  | Some LBRACE -> Stream.junk toks; let v1 = member_list toks in let () = rbrace toks in let v2 = values toks in v1 + v2
  | Some LBRACKET -> Stream.junk toks; let v1 = value_list toks in let () = rbracket toks in let v2 = values toks in v1 + v2
  | Some (DECIMAL _) -> Stream.junk toks; 1 + values toks
  | Some NULL -> Stream.junk toks; 1 + values toks
  | Some TRUE -> Stream.junk toks; 1 + values toks
  | Some FALSE -> Stream.junk toks; 1 + values toks
  | _ -> (* ε *) 0
end
