(*
 * Copyright (c) 2023 Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

let filenames = [
    (32, "../benchmarks/pgn/data/1610-1899_32k.pgn");
    (64, "../benchmarks/pgn/data/1610-1899_64k.pgn");
    (125, "../benchmarks/pgn/data/1610-1899_125k.pgn");
    (250, "../benchmarks/pgn/data/1610-1899_250k.pgn");
    (500, "../benchmarks/pgn/data/1610-1899_500k.pgn");
    (1024, "../benchmarks/pgn/data/1610-1899_1MB.pgn");
    (2048, "../benchmarks/pgn/data/1610-1899_2MB.pgn");
    (4160, "../benchmarks/pgn/data/1610-1899.pgn");
  ]
let args = List.map fst filenames
let files = List.map (fun (size, filename) -> (size, Core.In_channel.read_all filename)) filenames

let ocamlyacc_pgn n =
  let file = List.assoc n files in
  Core.Staged.stage (fun () -> Pgn_parser.start Pgn_lexer.token (Lexing.from_string file))

let menhir_code_pgn n =
  let file = List.assoc n files in
  Core.Staged.stage (fun () -> Pgn_parser_menhir_code.start Pgn_lexer_menhir_code.token (Lexing.from_string file))

let menhir_table_pgn n =
  let file = List.assoc n files in
  Core.Staged.stage (fun () -> Pgn_parser_menhir_table.start Pgn_lexer_menhir_table.token (Lexing.from_string file))

let ocamlyacc_normalized_pgn n =
  let file = List.assoc n files in
  Core.Staged.stage (fun () -> Pgn_normalized_parser.start Pgn_normalized_lexer.token (Lexing.from_string file))

let menhir_normalized_code_pgn n =
  let file = List.assoc n files in
  Core.Staged.stage (fun () -> Pgn_normalized_parser_menhir_code.start Pgn_normalized_lexer_menhir_code.token (Lexing.from_string file))

let menhir_normalized_table_pgn n =
  let file = List.assoc n files in
  Core.Staged.stage (fun () -> Pgn_normalized_parser_menhir_table.start Pgn_normalized_lexer_menhir_table.token (Lexing.from_string file))

let staged_pgn n =
  let file =  List.assoc n files in
  Core.Staged.stage (fun () -> Pgn_staged_combinator_parser.Parser.staged_complete file)

let unstaged_pgn n =
  let file = List.assoc n files in
  Core.Staged.stage (fun () -> Pgn_unstaged_combinator_parser.Parser.unstaged_complete file)

let fused_parser = Runnative.run Pgn_grammar.code

let fused_pgn n =
  let file = List.assoc n files in
  Core.Staged.stage (fun () -> fused_parser file)

let normalized_pgn n =
  let file = List.assoc n files in
  Core.Staged.stage (fun () -> Pgn_hand_normalized_parser.Unfused.games
                                 (Pgn_hand_normalized_lexer.stream_of_string file))

let _ = (* Check that all the parsers produce the same results *)
  let rec alleq : [`DRAWN | `OTHER | `WON] list list -> bool = function
    | [] | [_] -> true
    | x :: (x' :: _ as xs) -> x = x' && alleq xs in
  let run n (_name, p) = Core.Staged.unstage (p n) () in
  let parsers = ["yacc"         , ocamlyacc_pgn;
                 "menhir_code"  , menhir_code_pgn;
                 "menhir_table" , menhir_table_pgn;
                 "normalized_yacc"         , ocamlyacc_normalized_pgn;
                 "normalized_menhir_code"  , menhir_normalized_code_pgn;
                 "normalized_menhir_table" , menhir_normalized_table_pgn;
                 "staged"       , staged_pgn;
                 "unstaged"     , unstaged_pgn;
                 "fused"        , fused_pgn;
                 "normalized"        , normalized_pgn;
                ] in
  List.iter (fun n -> assert (alleq (List.map (run n) parsers))) args;
  Gc.compact ()

open Core
open Core_bench


let () =
  Command.run (Bench.make_command [
    Bench.Test.create_indexed ~name:"ocamlyacc_pgn" ~args
      ocamlyacc_pgn;

    Bench.Test.create_indexed ~name:"menhir_code_pgn" ~args
      menhir_code_pgn;

    Bench.Test.create_indexed ~name:"menhir_table_pgn" ~args
      menhir_table_pgn;

    Bench.Test.create_indexed ~name:"ocamlyacc_normalized_pgn" ~args
      ocamlyacc_normalized_pgn;

    Bench.Test.create_indexed ~name:"menhir_normalized_code_pgn" ~args
      menhir_normalized_code_pgn;

    Bench.Test.create_indexed ~name:"menhir_normalized_table_pgn" ~args
      menhir_normalized_table_pgn;

    Bench.Test.create_indexed ~name:"staged_pgn" ~args
      staged_pgn;
    
    (* Bench.Test.create_indexed ~name:"unstaged_pgn" ~args
     *   unstaged_pgn; *)

    Bench.Test.create_indexed ~name:"fused_pgn" ~args
      fused_pgn;

    Bench.Test.create_indexed ~name:"normalized_pgn" ~args
      normalized_pgn;
  ])
