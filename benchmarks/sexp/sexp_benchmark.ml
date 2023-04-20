(*
 * Copyright (c) 2023 Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

let filenames = [
    (262144, "../benchmarks/sexp/data/sexp.262144");
    (524288, "../benchmarks/sexp/data/sexp.524288");
    (786432, "../benchmarks/sexp/data/sexp.786432");
    (1048576, "../benchmarks/sexp/data/sexp.1048576");
    (1310720, "../benchmarks/sexp/data/sexp.1310720");
    (1572864, "../benchmarks/sexp/data/sexp.1572864");
    (1835008, "../benchmarks/sexp/data/sexp.1835008");
    (2097152, "../benchmarks/sexp/data/sexp.2097152");
  ]
let args = List.map fst filenames
let files = List.map (fun (size, filename) -> (size, Core.In_channel.read_all filename)) filenames

let ocamlyacc_sexp n =
  let file = List.assoc n files in
  Core.Staged.stage (fun () -> Sexp_parser.start Sexp_lexer.token (Lexing.from_string file))

let ocamlyacc_normalized_sexp n =
  let file = List.assoc n files in
  Core.Staged.stage (fun () -> Sexp_normalized_parser.start Sexp_normalized_lexer.token (Lexing.from_string file))

let menhir_code_sexp n =
  let file = List.assoc n files in
  Core.Staged.stage (fun () -> Sexp_parser_menhir_code.start Sexp_lexer_menhir_code.token (Lexing.from_string file))

let menhir_normalized_code_sexp n =
  let file = List.assoc n files in
  Core.Staged.stage (fun () -> Sexp_normalized_parser_menhir_code.start Sexp_normalized_lexer_menhir_code.token (Lexing.from_string file))

let menhir_table_sexp n =
  let file = List.assoc n files in
  Core.Staged.stage (fun () -> Sexp_parser_menhir_table.start Sexp_lexer_menhir_table.token (Lexing.from_string file))

let menhir_normalized_table_sexp n =
  let file = List.assoc n files in
  Core.Staged.stage (fun () -> Sexp_normalized_parser_menhir_table.start Sexp_normalized_lexer_menhir_table.token (Lexing.from_string file))

let staged_sexp n =
  let file = List.assoc n files in
  Core.Staged.stage (fun () -> Sexp_staged_combinator_parser.Parser.staged_complete file)

let unstaged_sexp n =
  let file = List.assoc n files in
  Core.Staged.stage (fun () -> Sexp_unstaged_combinator_parser.Parser.unstaged_complete file)

let parts_sexp n =
  let file = List.assoc n files in
  Core.Staged.stage (fun () -> Sexp_parts.parse (Sexp_parts.stream_of_string file))

let fused_parser = Runnative.run Sexp_grammar.code

let fused_sexp n =
  let file = List.assoc n files in
  Core.Staged.stage (fun () -> fused_parser file)

let normalized_sexp n =
  let file = List.assoc n files in
  Core.Staged.stage (fun () -> Sexp_hand_normalized_parser.Unfused.sexp
                                 (Sexp_hand_normalized_lexer.stream_of_string file))

let _ = (* Check that all the parsers produce the same results *)
  let rec alleq : int list -> bool = function
    | [] | [_] -> true
    | x :: (x' :: _ as xs) -> x = x' && alleq xs in
  let run n (_name, p) = Core.Staged.unstage (p n) () in
  let parsers = ["yacc"         , ocamlyacc_sexp;
                 "yacc"         , ocamlyacc_normalized_sexp;
                 (* "parts"        , parts_sexp; *)
                 "menhir_code"  , menhir_code_sexp;
                 "menhir_table" , menhir_table_sexp;
                 "staged"       , staged_sexp;
                 "unstaged"     , unstaged_sexp;
                 "fused"        , fused_sexp;
                 "normalized"   , normalized_sexp;
                 "menhir_normalized_code"  , menhir_normalized_code_sexp;
                 "menhir_normalized_table" , menhir_normalized_table_sexp;] in
  List.iter (fun n -> assert (alleq (List.map (run n) parsers))) args;
  Gc.compact ()

open Core
open Core_bench

let () =
  Command.run (Bench.make_command [
    Bench.Test.create_indexed ~name:"ocamlyacc_sexp" ~args
      ocamlyacc_sexp;
    
    Bench.Test.create_indexed ~name:"menhir_code_sexp" ~args
      menhir_code_sexp;
    
    Bench.Test.create_indexed ~name:"menhir_table_sexp" ~args
      menhir_table_sexp;

    Bench.Test.create_indexed ~name:"ocamlyacc_normalized_sexp" ~args
      ocamlyacc_normalized_sexp;
    
    Bench.Test.create_indexed ~name:"menhir_normalized_code_sexp" ~args
      menhir_normalized_code_sexp;
    
    Bench.Test.create_indexed ~name:"menhir_normalized_table_sexp" ~args
      menhir_normalized_table_sexp;

    Bench.Test.create_indexed ~name:"parts_sexp" ~args
      parts_sexp;
    
    Bench.Test.create_indexed ~name:"staged_sexp" ~args
      staged_sexp;

    (* Bench.Test.create_indexed ~name:"unstaged_sexp" ~args
     *   unstaged_sexp; *)

    Bench.Test.create_indexed ~name:"fused_sexp" ~args
      fused_sexp;

    Bench.Test.create_indexed ~name:"normalized_sexp" ~args
      normalized_sexp;
  ])
