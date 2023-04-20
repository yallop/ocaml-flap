(*
 * Copyright (c) 2023 Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open Csv_hand_normalized_parser
open Reex
open Benchmarks_common.Leex

let textdata = range '\x20' '\x21'
           <|> range '\x23' '\x2B'
           <|> range '\x2D' '\x7E'
let dquote = chr '"'
let doubledquote = dquote >>> dquote
let comma = chr ','
let cr = chr '\r'
let lf = chr '\n'
let crlf = cr >>> lf
let nonescaped = plus textdata (* TODO: not quite right: we should have star textdata here *)
let escaped    = dquote
             >>> star (textdata  <|> comma <|> cr <|> lf <|> doubledquote)
             >>> dquote

let lexer =[
 comma                  => (fun _ -> Return .<COMMA>.);
 crlf                   => (fun _ -> Return .<CRLF>.);
 escaped <|> nonescaped => (fun _ -> Return .<FIELD>.);
 epsilon                => (fun _ -> Return .<EOF>.);
]

let stream_of_lexer lexer s =
  let r = ref 0 in
  Stream.from (fun _ -> let tok, i = lexer s !r in r := i; Some tok)

let lexer_function = Runnative.run (Benchmarks_common.Leex.compile lexer)

let stream_of_string s =
  stream_of_lexer (fun s i -> lexer_function s ~start:i) s
