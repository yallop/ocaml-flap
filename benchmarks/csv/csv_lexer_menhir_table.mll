(*
 * Copyright (c) 2021 Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

{
open Csv_parser_menhir_table

}

let textdata = ['\x20'-'\x21' '\x23'-'\x2B' '\x2D'-'\x7E']
let dquote = '"'
let doubledquote = dquote dquote
let comma = ','
let cr = '\r'
let lf = '\n'
let crlf = cr lf
let nonescaped = textdata*
let escaped    = dquote (textdata | comma | cr | lf | doubledquote)* dquote
let field = escaped | nonescaped

rule token = parse
  | comma          { COMMA }
  | '\013' '\010'  { CRLF }
  | field as field { FIELD field }
  | _  as c        { Printf.ksprintf failwith "Unexpected: %c" c }
  | eof            { EOF }
{
}

