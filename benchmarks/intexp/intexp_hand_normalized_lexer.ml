(*
 * Copyright (c) 2023 Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open Intexp_hand_normalized_parser
open Reex
open Benchmarks_common.Leex

let lexer = [
    str "let"            => (fun _ -> Return .< LET>.);
    str "in"             => (fun _ -> Return .< IN>.);
    chr '*'              => (fun _ -> Return .< TIMES>.);
    chr '+'              => (fun _ -> Return .< PLUS>.);
    chr '-'              => (fun _ -> Return .< MINUS>.);
    chr '='              => (fun _ -> Return .< EQUAL>.);
    str "then"           => (fun _ -> Return .< THEN>.);
    str "else"           => (fun _ -> Return .< ELSE>.);
    str "if"             => (fun _ -> Return .< IF>.);
    chr '('              => (fun _ -> Return .< LPAREN>.);
    chr ')'              => (fun _ -> Return .< RPAREN>.);
    alpha >>> star alnum => (fun s -> Return .< ID .~s>.);
    plus digit           => (fun i -> Return .< INT (int_of_string .~i) >.);
    charset "\r\n \t"    => (fun _ -> Skip);
    epsilon              => (fun _ -> Return .<EOF>.);
  ]


let stream_of_lexer lexer s =
  let r = ref 0 in
  Stream.from (fun _ -> let tok, i = lexer s !r in r := i; Some tok)

let lexer_function = Runnative.run (Benchmarks_common.Leex.compile lexer)

let stream_of_string s =
  stream_of_lexer (fun s i -> lexer_function s ~start:i) s
