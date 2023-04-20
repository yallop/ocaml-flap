(*
 * Copyright (c) 2023 Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

let filenames = [
    (* 1, "../benchmarks/json/data/all.json" *)
    20, "../benchmarks/json/data/collect_20.json";
    40, "../benchmarks/json/data/collect_40.json";
    60, "../benchmarks/json/data/collect_60.json";
    80, "../benchmarks/json/data/collect_80.json";
    100, "../benchmarks/json/data/collect_100.json";
    120, "../benchmarks/json/data/collect_120.json";
    140, "../benchmarks/json/data/collect_140.json";
    160, "../benchmarks/json/data/collect_160.json";
    180, "../benchmarks/json/data/collect_180.json";
  ]
let args = List.map fst filenames
let files = List.map (fun (size, filename) -> (size, Core.In_channel.read_all filename)) filenames

let ocamlyacc_json n =
  let file = List.assoc n files in
  Core.Staged.stage (fun () -> Json_parser.start Json_lexer.token (Lexing.from_string file))

let menhir_code_json n =
  let file = List.assoc n files in
  Core.Staged.stage (fun () -> Json_parser_menhir_code.start Json_lexer_menhir_code.token (Lexing.from_string file))

let menhir_table_json n =
  let file = List.assoc n files in
  Core.Staged.stage (fun () -> Json_parser_menhir_table.start Json_lexer_menhir_table.token (Lexing.from_string file))

let ocamlyacc_normalized_json n =
  let file = List.assoc n files in
  Core.Staged.stage (fun () -> Json_normalized_parser.start Json_normalized_lexer.token (Lexing.from_string file))

let menhir_normalized_code_json n =
  let file = List.assoc n files in
  Core.Staged.stage (fun () -> Json_normalized_parser_menhir_code.start Json_normalized_lexer_menhir_code.token (Lexing.from_string file))

let menhir_normalized_table_json n =
  let file = List.assoc n files in
  Core.Staged.stage (fun () -> Json_normalized_parser_menhir_table.start Json_normalized_lexer_menhir_table.token (Lexing.from_string file))

let staged_json n =
  let file =  List.assoc n files in
  Core.Staged.stage (fun () -> Json_staged_combinator_parser.Parser.staged_complete file)

let unstaged_json n =
  let file = List.assoc n files in
  Core.Staged.stage (fun () -> Json_unstaged_combinator_parser.Parser.unstaged_complete file)

let parts_json n =
  let file = List.assoc n files in
  Core.Staged.stage (fun () -> Json_parts.parse (Json_parts.stream_of_string file))

let normalized_json n =
  let file = List.assoc n files in
  Core.Staged.stage (fun () -> Json_hand_normalized_parser.Unfused.values
                                 (Json_hand_normalized_lexer.stream_of_string file))

let fused_parser = Runnative.run Json_grammar.code

let fused_json n =
  let file = List.assoc n files in
  Core.Staged.stage (fun () -> fused_parser file)

let _ = (* Check that all the parsers produce the same results *)
  let rec alleq : int list -> bool = function
    | [] | [_] -> true
    | x :: (x' :: _ as xs) -> x = x' && alleq xs in
  let run n (_name, p) = Core.Staged.unstage (p n) () in
  let parsers = ["yacc"        , ocamlyacc_json;
                 "normalized_yacc"        , ocamlyacc_normalized_json;
                 "parts"       , parts_json;
                 "menhir_code" , menhir_code_json;
                 "menhir_table", menhir_table_json;
                 "menhir_normalized_code" , menhir_normalized_code_json;
                 "menhir_normalized_table", menhir_normalized_table_json;
                 "staged"      , staged_json;
                 "unstaged"    , unstaged_json;
                 "fused"       , fused_json;
                 "normalized"  , normalized_json] in
  List.iter (fun n -> assert (alleq (List.map (run n) parsers))) args;
  Gc.compact ()

open Core
open Core_bench


let () =
  Command.run (Bench.make_command [
    Bench.Test.create_indexed ~name:"ocamlyacc_json" ~args
      ocamlyacc_json;
    
    Bench.Test.create_indexed ~name:"menhir_code_json" ~args
      menhir_code_json;
    
    Bench.Test.create_indexed ~name:"menhir_table_json" ~args
      menhir_table_json;
    
    Bench.Test.create_indexed ~name:"ocamlyacc_normalized_json" ~args
      ocamlyacc_normalized_json;
    
    Bench.Test.create_indexed ~name:"menhir_normalized_code_json" ~args
      menhir_normalized_code_json;
    
    Bench.Test.create_indexed ~name:"menhir_normalized_table_json" ~args
      menhir_normalized_table_json;
    
    Bench.Test.create_indexed ~name:"normalized_json" ~args
      normalized_json;

    Bench.Test.create_indexed ~name:"parts_json" ~args
      parts_json;
    
    Bench.Test.create_indexed ~name:"staged_json" ~args
      staged_json;

    (* Bench.Test.create_indexed ~name:"unstaged_json" ~args
     *   unstaged_json; *)

    Bench.Test.create_indexed ~name:"fused_json" ~args
      fused_json;
  ])
