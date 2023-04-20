(*
 * Copyright (c) 2023 Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open Pgn_hand_normalized_parser
open Reex
open Benchmarks_common.Leex

let lexer = [
    chr '['                           => (fun _ -> Return .<LBRACKET>.);
    regex "]"                         => (fun _ -> Return .<RBRACKET>.);
    regex "/"                         => (fun _ -> Return .<SLASH>.);
    regex "-"                         => (fun _ -> Return .<MINUS>.);
    regex "[*]"                       => (fun _ -> Return .<STAR>.);
    string                            => (fun s -> Return .<STRING .~s>.);
    plus alpha >>> digit >>> 
      star (charset "=#-+" <|> alnum) => (fun s -> Return .<COORDINATE .~s>.);
    regex "O-O(-O)?([#+])?"           => (fun _ -> Return .<CASTLE>.);
    plus digit >>> chr '.'            => (fun s -> Return .<INTDOT .~s>.);
    plus digit                        => (fun s -> Return .<INT .~s>.);
    upper >>> star alpha              => (fun s -> Return .<TAG .~s>.);
    regex"[\n]"                       => (fun _ -> Skip);
    regex"[\r\n \t]"                  => (fun _ -> Skip);
    epsilon                           => (fun _ -> Return .<EOF>.);
    (* reject any "Unexpected character" *)]


let stream_of_lexer lexer s =
  let r = ref 0 in
  Stream.from (fun _ -> let tok, i = lexer s !r in r := i; Some tok)

let lexer_function = Runnative.run (Benchmarks_common.Leex.compile lexer)

let stream_of_string s =
  stream_of_lexer (fun s i -> lexer_function s ~start:i) s
