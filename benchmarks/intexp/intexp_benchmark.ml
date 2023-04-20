(*
 * Copyright (c) 2023 Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

let filenames = [
    (262144, ("../benchmarks/intexp/data/intexp.262144", 2491880767779407440));
    (524288, ("../benchmarks/intexp/data/intexp.524288", -4445224671334544474));
    (786432, ("../benchmarks/intexp/data/intexp.786432", -850148006049275809));
    (1048576, ("../benchmarks/intexp/data/intexp.1048576", 0));
    (1310720, ("../benchmarks/intexp/data/intexp.1310720", 660851688233856212));
    (1572864, ("../benchmarks/intexp/data/intexp.1572864", 0));
    (1835008, ("../benchmarks/intexp/data/intexp.1835008", 1));
    (2097152, ("../benchmarks/intexp/data/intexp.2097152", 2775427195430798160));
  ]
let args = List.map fst filenames
let files = List.map (fun (size, (filename, expected)) -> (size, (Core.In_channel.read_all filename, expected))) filenames

let ocamlyacc_intexp n =
  let exp, expected =  List.assoc n files in
  Core.Staged.stage (fun () ->
      let result = Intexp_parser.start Intexp_lexer.token (Lexing.from_string exp)
      in assert (result [] = expected))

let menhir_code_intexp n =
  let exp, expected =  List.assoc n files in
  Core.Staged.stage (fun () ->
      let result = Intexp_parser_menhir_code.start Intexp_lexer_menhir_code.token (Lexing.from_string exp)
      in assert (result [] = expected))

let menhir_table_intexp n =
  let exp, expected =  List.assoc n files in
  Core.Staged.stage (fun () ->
      let result = Intexp_parser_menhir_table.start Intexp_lexer_menhir_table.token (Lexing.from_string exp)
      in assert (result [] = expected))

let ocamlyacc_normalized_intexp n =
  let exp, expected =  List.assoc n files in
  Core.Staged.stage (fun () ->
      let result = Intexp_normalized_parser.start Intexp_normalized_lexer.token (Lexing.from_string exp)
      in assert (result [] = expected))

let menhir_normalized_code_intexp n =
  let exp, expected =  List.assoc n files in
  Core.Staged.stage (fun () ->
      let result = Intexp_normalized_parser_menhir_code.start Intexp_normalized_lexer_menhir_code.token (Lexing.from_string exp)
      in assert (result [] = expected))

let menhir_normalized_table_intexp n =
  let exp, expected =  List.assoc n files in
  Core.Staged.stage (fun () ->
      let result = Intexp_normalized_parser_menhir_table.start Intexp_normalized_lexer_menhir_table.token (Lexing.from_string exp)
      in assert (result [] = expected))

let staged_intexp n =
  let exp, expected =  List.assoc n files in
  Core.Staged.stage (fun () ->
      let result = Intexp_staged_combinator_parser.Parser.staged_complete exp
      in assert (result [] = expected))

let unstaged_intexp n =
  let exp, expected =  List.assoc n files in
  Core.Staged.stage (fun () ->
      let result = Intexp_unstaged_combinator_parser.Parser.unstaged_complete exp
      in assert (result [] = expected))

let fused_parser = Runnative.run Intexp_grammar.code

let fused_intexp n =
  let exp, expected = List.assoc n files in
  Core.Staged.stage (fun () ->
      let result = fused_parser exp
      in assert (result [] = expected))

let normalized_intexp n =
  let exp, expected = List.assoc n files in
  Core.Staged.stage (fun () ->
      let result = 
        Intexp_hand_normalized_parser.Unfused.exp
          (Intexp_hand_normalized_lexer.stream_of_string exp)
      in
      assert (result [] = expected))


open Core
open Core_bench

let () =
  Command.run (Bench.make_command [
    Bench.Test.create_indexed ~name:"ocamlyacc_intexp" ~args
      ocamlyacc_intexp;

    Bench.Test.create_indexed ~name:"menhir_code_intexp" ~args
      menhir_code_intexp;

    Bench.Test.create_indexed ~name:"menhir_table_intexp" ~args
      menhir_table_intexp;

    Bench.Test.create_indexed ~name:"ocamlyacc_normalized_intexp" ~args
      ocamlyacc_normalized_intexp;

    Bench.Test.create_indexed ~name:"menhir_normalized_code_intexp" ~args
      menhir_normalized_code_intexp;

    Bench.Test.create_indexed ~name:"menhir_normalized_table_intexp" ~args
      menhir_normalized_table_intexp;

    Bench.Test.create_indexed ~name:"staged_intexp" ~args
      staged_intexp;
    
    Bench.Test.create_indexed ~name:"fused_intexp" ~args
      fused_intexp;

    Bench.Test.create_indexed ~name:"normalized_intexp" ~args
      normalized_intexp;

    (* Bench.Test.create_indexed ~name:"unstaged_intexp" ~args
     *   unstaged_intexp; *)
  ])
