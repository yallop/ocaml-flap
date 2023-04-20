(*
 * Copyright (c) 2023 Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

type tok = INT of int | P3 | EOF

(* DGNF grammar produced by flap:
<image,
{ints ↦ INT ints | ε,
 int ↦  INT,
 image ↦ P3 int int int ints,}>
 *)

module Unfused =
struct
  let rec int toks =
    match Stream.peek toks with
    | Some (INT x) -> Stream.junk toks; x
    | _ -> failwith "parsing error"

  and ints toks =
    match Stream.peek toks with
    | Some (INT x1) -> Stream.junk toks; 
                       let p2 = ints toks in
                       (max x1 (fst p2), succ (snd p2))
    | _ -> (* ε lookahead *) (min_int, 0)

  and image toks =
    match Stream.peek toks with
    | Some P3 -> Stream.junk toks;
                 let w = int toks in
                 let h = int toks in
                 let max = int toks in
                 let max', count = ints toks in
                 (max' <= max) && (count = 3 * w * h)
    | _ -> failwith "parsing error"
end
