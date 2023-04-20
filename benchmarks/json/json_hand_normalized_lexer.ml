(*
 * Copyright (c) 2023 Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open Json_hand_normalized_parser
open Reex
open Benchmarks_common.Leex

let lexer = [
       chr '['           => (fun _ -> Return .<LBRACKET>.);
       chr ']'           => (fun _ -> Return .<RBRACKET>.);
       chr '{'           => (fun _ -> Return .<LBRACE>.);
       chr '}'           => (fun _ -> Return .<RBRACE>.);
       chr ','           => (fun _ -> Return .<COMMA>.);
       chr ':'           => (fun _ -> Return .<COLON>.);
       str "null"        => (fun _ -> Return .<NULL>.);
       str "true"        => (fun _ -> Return .<TRUE>.);
       str "false"       => (fun _ -> Return .<FALSE>.);
       string            => (fun s -> Return .<STRING .~s>.);
       decimal           => (fun s -> Return .<DECIMAL .~s>.);
       charset "\r\n \t" => (fun _ -> Skip);
       epsilon           => (fun _ -> Return .<EOF>.);
  ]

let stream_of_lexer lexer s =
  let r = ref 0 in
  Stream.from (fun _ -> let tok, i = lexer s !r in r := i; Some tok)

let lexer_function = Runnative.run (Benchmarks_common.Leex.compile lexer)

let stream_of_string s =
  stream_of_lexer (fun s i -> lexer_function s ~start:i) s
