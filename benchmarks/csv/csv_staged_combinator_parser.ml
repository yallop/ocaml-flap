(*
 * Copyright (c) 2023 Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

module P1 = Asp.Staged.Parse(Asp.Utilities.Unstaged.Char_element)
module P2 = Asp.Staged.Parse(Csv_tokens)

module Lexer = struct
  open P1
  open Asp.Utilities.Staged.Charparsing

  let lex : Csv_tokens_base.t option t =
    let textdata = charset "\x20\x21\x23\x24\x25\x26\x27\x28\x29\x2A\x2B\x2D\x7E" in
    let dquote = chr '"' in
    let doubledquote = (dquote >>> dquote) $ fun _ -> .<'"'>. in
    let comma = chr ',' in
    let cr = chr '\r' in
    let lf = chr '\n' in
    let crlf = cr >>> lf in
    let nonescaped = star textdata in
    let escaped    = (dquote (* TODO: there's a conflict between doublequote and dquote here *)
                 >>> star (textdata  <|> comma <|> cr <|> lf <|> doubledquote)
                 >>> dquote) $ fun s -> .< let (_,s),_ = .~s in s >. in
    let field = escaped <|> nonescaped in
    (comma $ fun _ -> .< Some (Csv_tokens_base.T (Csv_tokens_base.COMMA, ())) >.)
    <|>
    (crlf $ fun _ -> .< Some (Csv_tokens_base.T (Csv_tokens_base.CRLF, ())) >.)
    <|>
    (field $ fun s -> .< Some (Csv_tokens_base.T (Csv_tokens_base.FIELD, .~s)) >.)

  (* TODO: move all the following code to a library *)
  let lexcode =
    let module R = P1.Parser(Asp_streamcode.Stringcode) in
    R.compile (type_check lex)

  let staged_lexer = Runnative.run lexcode

  let next s =
    let i = ref 0 in
    fun _ ->
    let tok, i' = staged_lexer ~index:!i s in
    i := i';
    tok

  let staged_lexer_stream : string -> Csv_tokens_base.t Stream.t =
    fun s -> Stream.from (next s)
end

module Parser =
struct
  open P2
  open Csv_tokens_base

  let field = tok FIELD $ fun _ -> .<1>.
  let comma = tok COMMA
  let crlf = tok CRLF

  let fields = fix @@ fun fields -> (eps .<0>.)
                                <|> ((comma >>> field) >>> fields $ fun p -> .< 1 + (snd .~p) >.)

  let record = (field >>> fields) >>> crlf $ fun p -> .< let (_,p),_ = .~p in 1 + p >.
  let records = fix @@ fun records -> (record >>> maybe records $ fun p -> 
                                        .< match .~p with r, None -> r
                                                        | r, Some rs -> (assert (r = rs); r) >.)
  let file = records

  (* TODO: move all the following code to a library *)
  let parsecode =
    let module R = P2.Parser(Asp_streamcode.Streamcode(struct type t = Csv_tokens_base.t end)) in
    R.compile (type_check file)

  let staged_parser : Csv_tokens_base.t Stream.t -> int
    = Runnative.run parsecode

  let staged_complete : string -> int =
    fun s -> staged_parser (Lexer.staged_lexer_stream s)
end
