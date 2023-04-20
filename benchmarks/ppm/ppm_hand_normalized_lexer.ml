(*
 * Copyright (c) 2023 Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open Ppm_hand_normalized_parser
open Reex
open Benchmarks_common.Leex

let lexer = [
    str "P3"                          => (fun _ -> Return .<P3>.);
    plus digit                        => (fun s -> Return .<INT (int_of_string .~s)>.);
    chr '#' >>> star (not (chr '\n')) => (fun _ -> Skip);
    chr '\n'                          => (fun _ -> Skip);
    charset "\r\n \t"                 => (fun _ -> Skip);
    any                               => (fun _ -> Error "Unexpected character");
    epsilon                           => (fun _ -> Return .<EOF>.)
  ]

let stream_of_lexer lexer s =
  let r = ref 0 in
  Stream.from (fun _ -> let tok, i = lexer s !r in r := i; Some tok)

let lexer_function = Runnative.run (Benchmarks_common.Leex.compile lexer)

let stream_of_string s =
  stream_of_lexer (fun s i -> lexer_function s ~start:i) s
