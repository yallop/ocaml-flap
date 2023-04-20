(*
 * Copyright (c) 2023 Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

type tok = ATOM of string | LPAREN  | RPAREN  | EOF

(* DGNF grammar produced by flap:

start: sexp

rpar ↦ RPAREN

sexps ↦ LPAREN sexps rpar sexps
       | ATOM sexps
       | ε,

sexp ↦ LPAREN sexps rpar
       | ATOM
   
*)

module Unfused =
struct
  let rpar toks =
    match Stream.peek toks with
    | Some RPAREN -> Stream.junk toks; ()
    | _ -> failwith "parsing error"

  let rec sexps toks =
    match Stream.peek toks  with
    | Some (ATOM _) -> Stream.junk toks;
                       let s = sexps toks in 
                       1 + s
    | Some LPAREN -> Stream.junk toks;
                     let s1 = sexps toks in
                     let () = rpar toks in
                     let s2 = sexps toks in
                     s1 + s2
    | _ -> (* lookahead: don't junk *) 0
            
  let sexp toks =
    match Stream.peek toks with
    | Some (ATOM _) -> Stream.junk toks; 1
    | Some LPAREN   -> Stream.junk toks; let s = sexps toks in let () = rpar toks in s
    | Some EOF      -> Stream.junk toks; 0
    | _ -> assert false 
end
