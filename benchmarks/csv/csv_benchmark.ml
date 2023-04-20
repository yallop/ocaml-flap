(*
 * Copyright (c) 2023 Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

let filenames = [
    16, "../benchmarks/csv/data/16k.csv";
    32, "../benchmarks/csv/data/32k.csv";
    64, "../benchmarks/csv/data/64k.csv";
    128, "../benchmarks/csv/data/128k.csv";
    256, "../benchmarks/csv/data/256k.csv";
    512, "../benchmarks/csv/data/512k.csv";
  ]
let args = List.map fst filenames
let files = List.map (fun (size, filename) -> (size, Core.In_channel.read_all filename)) filenames

let ocamlyacc_csv n =
  let file = List.assoc n files in
  Core.Staged.stage (fun () ->
      Csv_parser.file Csv_lexer.token (Lexing.from_string file))

let menhir_code_csv n =
  let file = List.assoc n files in
  Core.Staged.stage (fun () ->
      Csv_parser_menhir_code.file Csv_lexer_menhir_code.token (Lexing.from_string file))

let menhir_table_csv n =
  let file = List.assoc n files in
  Core.Staged.stage (fun () ->
      Csv_parser_menhir_table.file Csv_lexer_menhir_table.token (Lexing.from_string file))

let ocamlyacc_normalized_csv n =
  let file = List.assoc n files in
  Core.Staged.stage (fun () ->
      Csv_normalized_parser.file Csv_normalized_lexer.token (Lexing.from_string file))

let menhir_normalized_code_csv n =
  let file = List.assoc n files in
  Core.Staged.stage (fun () ->
      Csv_normalized_parser_menhir_code.file Csv_normalized_lexer_menhir_code.token (Lexing.from_string file))

let menhir_normalized_table_csv n =
  let file = List.assoc n files in
  Core.Staged.stage (fun () ->
      Csv_normalized_parser_menhir_table.file Csv_normalized_lexer_menhir_table.token (Lexing.from_string file))

(* let staged_csv n =
 *   let file = List.assoc n files in
 *   Core.Staged.stage (fun () ->
 *       Csv_staged_combinator_parser.Parser.staged_complete file) *)

let fused_parser = Runnative.run Csv_grammar.code

let fused_csv n =
  let file = List.assoc n files in
  Core.Staged.stage (fun () -> fused_parser file)

let normalized_csv n =
  let file = List.assoc n files in
  Core.Staged.stage (fun () -> Csv_hand_normalized_parser.Unfused.file
                                 (Csv_hand_normalized_lexer.stream_of_string file))

let _ = (* Check that all the parsers produce the same results *)
  let rec alleq : int list -> bool = function
    | [] | [_] -> true
    | x :: (x' :: _ as xs) -> x = x' && alleq xs in
  let run n (_name, p) = Core.Staged.unstage (p n) () in
  let parsers = ["yacc"         , ocamlyacc_csv;
                 "menhir_code"  , menhir_code_csv;
                 "menhir_table" , menhir_table_csv;
                 "normalized_yacc"         , ocamlyacc_normalized_csv;
                 "normalized_menhir_code"  , menhir_normalized_code_csv;
                 "normalized_menhir_table" , menhir_normalized_table_csv;
                 (* "staged"   , staged_csv; *)
                 "fused"    , fused_csv;
                 "normalized", normalized_csv] in
  List.iter (fun n -> assert (alleq (List.map (run n) parsers))) args;
  Gc.compact ()

open Core
open Core_bench

let () =
  Command.run (Bench.make_command [
    Bench.Test.create_indexed ~name:"ocamlyacc_csv" ~args
      ocamlyacc_csv;

    Bench.Test.create_indexed ~name:"menhir_code_csv" ~args
      menhir_code_csv;

    Bench.Test.create_indexed ~name:"menhir_table_csv" ~args
      menhir_table_csv;

    Bench.Test.create_indexed ~name:"ocamlyacc_normalized_csv" ~args
      ocamlyacc_normalized_csv;

    Bench.Test.create_indexed ~name:"menhir_normalized_code_csv" ~args
      menhir_normalized_code_csv;

    Bench.Test.create_indexed ~name:"menhir_normalized_table_csv" ~args
      menhir_normalized_table_csv;

    (* Bench.Test.create_indexed ~name:"staged_csv" ~args
     *   staged_csv; *)

    (* Bench.Test.create_indexed ~name:"unstaged_csv" ~args
     *   unstaged_csv; *)

    Bench.Test.create_indexed ~name:"fused_csv" ~args
      fused_csv;

    Bench.Test.create_indexed ~name:"normalized_csv" ~args
      normalized_csv;
  ])
