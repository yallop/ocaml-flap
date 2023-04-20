(*
 * Copyright (c) 2023 Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open Reex

type 'f rhs = Skip | Error of string | Return of 'f code
type 'a case = Case of Reex.t * (string code -> 'a rhs)

let (=>) lhs rhs = Case (lhs, rhs)

let compile cases =
  .< let rec lex ?(start=0) s =
     .~(Reex_match.match_ .<start>. .<s>.
          (List.rev @@ fst @@ List.fold_left
            (fun (cases, seen) (Case (lhs, rhs)) ->
              let lhs' = lhs <&> not seen in
              let rhs' _ ~index:i ~len:_ _ = match rhs .<String.sub s start (.~i - start) >. with
                | Skip     -> .< lex ~start:.~i s >.
                | Error s  -> .< failwith s >.
                | Return v -> .< (.~v, .~i) >. in
              ((lhs', rhs') :: cases, seen <|> lhs'))
             ([], empty)
             cases))
     in lex >.

let upper = range 'A' 'Z'
let lower = range 'a' 'z'
let alpha = upper <|> lower
let digit = range '0' '9'
let alnum = alpha <|> digit
let charset s = Seq.fold_left (fun s c -> s <|> chr c) empty (String.to_seq s)
let decimal = plus digit >>> opt (chr '.' >>> plus digit)

let complement s = any <&> not (charset s)
let stringchar = complement "\"\\" <|> (chr '\\' >>> any)
let string = chr '"' >>> star stringchar >>> chr '"'
