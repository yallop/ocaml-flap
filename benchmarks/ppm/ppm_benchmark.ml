(*
 * Copyright (c) 2023 Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

let filenames = [
    (10*10, "../benchmarks/ppm/data/popl2019_twitter_avatar_10x10.ppm");
    (14*14, "../benchmarks/ppm/data/popl2019_twitter_avatar_14x14.ppm");
    (20*20, "../benchmarks/ppm/data/popl2019_twitter_avatar_20x20.ppm");
    (28*28, "../benchmarks/ppm/data/popl2019_twitter_avatar_28x28.ppm");
    (40*40, "../benchmarks/ppm/data/popl2019_twitter_avatar_40x40.ppm");
    (56*56, "../benchmarks/ppm/data/popl2019_twitter_avatar_56x56.ppm");
    (80*80, "../benchmarks/ppm/data/popl2019_twitter_avatar_80x80.ppm");
    (113*113, "../benchmarks/ppm/data/popl2019_twitter_avatar_113x113.ppm");
    (160*160, "../benchmarks/ppm/data/popl2019_twitter_avatar_160x160.ppm");
    (226*226, "../benchmarks/ppm/data/popl2019_twitter_avatar_226x226.ppm");
  ]
let args = List.map fst filenames
let files = List.map (fun (size, filename) -> (size, Core.In_channel.read_all filename)) filenames

let ocamlyacc_ppm n =
  let file = List.assoc n files in
  Core.Staged.stage (fun () ->
      assert (Ppm_parser.image Ppm_lexer.token (Lexing.from_string file)))

let menhir_code_ppm n =
  let file = List.assoc n files in
  Core.Staged.stage (fun () ->
      assert (Ppm_parser_menhir_code.image Ppm_lexer_menhir_code.token (Lexing.from_string file)))

let menhir_table_ppm n =
  let file = List.assoc n files in
  Core.Staged.stage (fun () ->
      assert (Ppm_parser_menhir_table.image Ppm_lexer_menhir_table.token (Lexing.from_string file)))

let ocamlyacc_normalized_ppm n =
  let file = List.assoc n files in
  Core.Staged.stage (fun () ->
      assert (Ppm_normalized_parser.image Ppm_normalized_lexer.token (Lexing.from_string file)))

let menhir_normalized_code_ppm n =
  let file = List.assoc n files in
  Core.Staged.stage (fun () ->
      assert (Ppm_normalized_parser_menhir_code.image Ppm_normalized_lexer_menhir_code.token (Lexing.from_string file)))

let menhir_normalized_table_ppm n =
  let file = List.assoc n files in
  Core.Staged.stage (fun () ->
      assert (Ppm_normalized_parser_menhir_table.image Ppm_normalized_lexer_menhir_table.token (Lexing.from_string file)))


let staged_ppm n =
  let file = List.assoc n files in
  Core.Staged.stage (fun () ->
      assert (Ppm_staged_combinator_parser.Parser.staged_complete file))

let unstaged_ppm n =
  let file = List.assoc n files in
  Core.Staged.stage (fun () ->
      assert (Ppm_unstaged_combinator_parser.Parser.unstaged_complete file))

let fused_parser = Runnative.run Ppm_grammar.code

let fused_ppm n =
  let file = List.assoc n files in
  Core.Staged.stage (fun () -> assert (fused_parser file))

let normalized_ppm n =
  let file = List.assoc n files in
  Core.Staged.stage (fun () -> assert (Ppm_hand_normalized_parser.Unfused.image
                                         (Ppm_hand_normalized_lexer.stream_of_string file)))

open Core
open Core_bench

let () =
  Command.run (Bench.make_command [
    Bench.Test.create_indexed ~name:"ocamlyacc_ppm" ~args
      ocamlyacc_ppm;

    Bench.Test.create_indexed ~name:"menhir_code_ppm" ~args
      menhir_code_ppm;

    Bench.Test.create_indexed ~name:"menhir_table_ppm" ~args
      menhir_table_ppm;

    Bench.Test.create_indexed ~name:"ocamlyacc_normalized_ppm" ~args
      ocamlyacc_normalized_ppm;

    Bench.Test.create_indexed ~name:"menhir_normalized_code_ppm" ~args
      menhir_normalized_code_ppm;

    Bench.Test.create_indexed ~name:"menhir_normalized_table_ppm" ~args
      menhir_normalized_table_ppm;

    Bench.Test.create_indexed ~name:"staged_ppm" ~args
      staged_ppm;

    (* Bench.Test.create_indexed ~name:"unstaged_ppm" ~args
     *   unstaged_ppm; *)

    Bench.Test.create_indexed ~name:"fused_ppm" ~args
      fused_ppm;

    Bench.Test.create_indexed ~name:"normalized_ppm" ~args
      normalized_ppm;
  ])
