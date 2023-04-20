open Sexp_hand_normalized_parser
open Reex
open Benchmarks_common.Leex

let lexer =
  [ alpha >>> star alnum => (fun s -> Return .<ATOM .~s>.);
    charset "\r\n \t"    => (fun _ -> Skip);
    chr '('              => (fun _ -> Return .<LPAREN>.);
    chr ')'              => (fun _ -> Return .<RPAREN>.);
  ]

let stream_of_lexer lexer s =
  let r = ref 0 in
  Stream.from (fun _ -> let tok, i = lexer s !r in r := i; Some tok)

let lexer_function = Runnative.run (Benchmarks_common.Leex.compile lexer)

let stream_of_string s =
  stream_of_lexer (fun s i -> lexer_function s ~start:i) s
