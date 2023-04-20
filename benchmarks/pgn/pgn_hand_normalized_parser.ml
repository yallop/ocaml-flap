(*
 * Copyright (c) 2023 Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

type tok = COORDINATE of string | CASTLE | STAR | INT of string | INTDOT of string | MINUS | SLASH
         | STRING of string | LBRACKET | RBRACKET | TAG of string | EOF

(* DGNF grammar produced by flap:
<games,
{int ↦ INT,
slash ↦ SLASH,
minus ↦ MINUS,
rbracket ↦ RBRACKET,
string ↦ STRING,
tag ↦ TAG,
result ↦ STAR  | INT result_rest,
moves ↦ INTDOT coordinate coordinate_opt moves | ε,
games_opt ↦  LBRACKET tag string rbracket metadata moves result games_opt 
       | ε,
result_rest ↦ MINUS int  
       | SLASH int minus int slash int,
coordinate_opt ↦ COORDINATE  
      | CASTLE
      | ε,
coordinate ↦ COORDINATE  
       | CASTLE,
metadata ↦ LBRACKET tag string rbracket metadata  | ε,
games ↦ LBRACKET tag string rbracket metadata moves result games_opt
}>
 *)

module Unfused =
struct
  let rec int toks =
    match Stream.peek toks with
    | Some (INT i) -> Stream.junk toks; i
    | _ -> failwith "parsing error"

  and slash toks =
    match Stream.peek toks with
    | Some SLASH -> Stream.junk toks; ()
    | _ -> failwith "parsing error"

  and minus toks =
    match Stream.peek toks with
    | Some MINUS -> Stream.junk toks; ()
    | _ -> failwith "parsing error"

  and rbracket toks =
    match Stream.peek toks with
    | Some RBRACKET -> Stream.junk toks; ()
    | _ -> failwith "parsing error"

  and string toks =
    match Stream.peek toks with
    | Some (STRING s) -> Stream.junk toks; s
    | _ -> failwith "parsing error"

  and tag toks =
    match Stream.peek toks with
    | Some (TAG t) -> Stream.junk toks; t
    | _ -> failwith "parsing error"

  and result_rest toks =
    match Stream.peek toks with
    | Some MINUS -> Stream.junk toks; let _ = int toks in `WON
    | Some SLASH -> Stream.junk toks; let _ = int toks in let () = minus toks in let _ = int toks in let () = slash toks in let _ = int toks in `DRAWN
    | _ -> failwith "parsing error"

  and result toks =
    match Stream.peek toks with
    | Some STAR -> Stream.junk toks; `OTHER
    | Some (INT _) -> Stream.junk toks; result_rest toks
    | _ -> failwith "parsing error"

  and coordinate_opt toks =
    match Stream.peek toks with
    | Some (COORDINATE x) -> Stream.junk toks; Some (`COORD x)
    | Some CASTLE -> Stream.junk toks; Some `CASTLE
    | _ -> (* ε *) None

  and moves toks =
    match Stream.peek toks with
    | Some (INTDOT _) -> Stream.junk toks; let x = coordinate toks in
                                           let y =  coordinate_opt toks in
                                           let xs = moves toks in
                                           (x,y) :: xs
    | _ -> (* ε *) []

  and coordinate toks =
    match Stream.peek toks with
    | Some (COORDINATE c) -> Stream.junk toks; (`COORD c)
    | Some CASTLE -> Stream.junk toks; `CASTLE
    | _ -> failwith "parsing error"

  and metadata toks =
    match Stream.peek toks with
    | Some LBRACKET -> Stream.junk toks;
                       let x = tag toks in
                       let y = string toks in
                       let () = rbracket toks in
                       let ys = metadata toks in
                       (x,y) :: ys
    | _ -> (* ε *) []

  and games_opt toks =
    match Stream.peek toks with
    | Some LBRACKET -> Stream.junk toks;
                       let x = tag toks in
                       let y = string toks in
                       let () = rbracket toks in
                       let md = metadata toks in
                       let _mv = moves toks in
                       let r = result toks in
                       let go = games_opt toks in
                       let _ = (x,y) :: md in
                       r :: go
    | _ -> (* ε *) []

  and games toks =
    match Stream.peek toks with
    | Some LBRACKET -> Stream.junk toks;
                       let x = tag toks in
                       let y = string toks in
                       let () = rbracket toks in
                       let md = metadata toks in
                       let _mv = moves toks in
                       let r = result toks in
                       let go = games_opt toks in
                       let _ = (x,y) :: md in
                       r :: go
    | _ -> failwith "parsing error"
end
