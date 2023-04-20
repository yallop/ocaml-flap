(*
 * Copyright (c) 2023 Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

type tok = CRLF | COMMA | FIELD | EOF

(*
  <file,
{recordStar ↦ FIELD rest crlf recordStar | ε,
crlf ↦ CRLF,
field ↦ FIELD,
rest ↦ COMMA field rest | ε,
file ↦ FIELD rest crlf recordStar, }>
 *)
module Unfused =
struct
  let rec recordStar toks =
    match Stream.peek toks with
    | Some FIELD -> Stream.junk toks;
                    let v1 = rest toks in
                    let () = crlf toks in
                    (match recordStar toks with
                     | None -> Some (1+v1)
                     | Some r -> assert (1+v1 = r); Some r)
    | _ -> (* ε *) None

  and crlf toks =
    match Stream.peek toks with
    | Some CRLF -> Stream.junk toks; ()
    | _ -> failwith "parsing error"

  and field toks =
    match Stream.peek toks with
    | Some FIELD -> Stream.junk toks; ()
    | _ -> failwith "parsing error"

  and rest toks =
    match Stream.peek toks with
    | Some COMMA -> Stream.junk toks; field toks; 1 + rest toks
    | _ -> (* ε *) 0

  and file toks =
    match Stream.peek toks with
    | Some FIELD -> Stream.junk toks; let v1 = rest toks in
                                      let () = crlf toks in
                                      (match recordStar toks with
                                       | None -> (1 + v1)
                                       | Some r -> assert (1 + v1 = r); r)
    | _ -> failwith "parsing error"
end
