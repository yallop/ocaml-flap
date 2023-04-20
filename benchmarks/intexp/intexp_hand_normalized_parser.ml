(*
 * Copyright (c) 2023 Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

type tok = LET | ID of string | IN | INT of int | TIMES | PLUS | MINUS | EQUAL | THEN | ELSE | IF | LPAREN | RPAREN | EOF


(* DGNF grammar produced by flap:

<exp,
{else_ ↦ ELSE,
then_ ↦ THEN,
in_ ↦ IN,
equal_ ↦ EQUAL,
id ↦ ID,
rparen ↦ RPAREN,
atom ↦ ID
         | INT 
         | LPAREN exp rparen,
timeses ↦ TIMES atom timeses
         | ε,
timesexp ↦ ID timeses  
         | INT timeses  
         | LPAREN exp rparen timeses,
pluses ↦ PLUS timesexp pluses  
         | MINUS timesexp pluses
         | ε,
times_plus ↦ ID timeses pluses  
         | INT timeses pluses  
         | LPAREN exp rparen timeses pluses,
equalses ↦ EQUAL times_plus equalses
         | ε,
exp:
         | LET id equal_ exp in_ exp  
         | ID timeses pluses equalses  
         | INT timeses pluses equalses  
         | IF exp then_ exp else_ exp  
         | LPAREN exp rparen timeses pluses equalses,
}>
*)
module Unfused =
struct

  let rec else_ toks =
    match Stream.peek toks with
    | Some ELSE -> Stream.junk toks; ()
    | _ -> failwith "parsing error"

  and then_ toks =
    match Stream.peek toks with
    | Some THEN -> Stream.junk toks; ()
    | _ -> failwith "parsing error"

  and in_ toks =
    match Stream.peek toks with
    | Some IN -> Stream.junk toks; ()
    | _ -> failwith "parsing error"

  and equal_ toks =
    match Stream.peek toks with
    | Some EQUAL -> Stream.junk toks; ()
    | _ -> failwith "parsing error"

  and id toks =
    match Stream.peek toks with
    | Some (ID x) -> Stream.junk toks; x
    | _ -> failwith "parsing error"

  and rparen toks =
    match Stream.peek toks with
    | Some RPAREN -> Stream.junk toks; ()
    | _ -> failwith "parsing error"

  and atom toks =
    match Stream.peek toks with
    | Some (ID x) -> Stream.junk toks; List.assoc x
    | Some (INT i) -> Stream.junk toks; (fun _ -> i)
    | Some LPAREN -> Stream.junk toks; let v = exp toks in
                                       let () = rparen toks in
                                       v
    | _ -> failwith "parsing error"

  and timeses toks =
    match Stream.peek toks with
    | Some TIMES -> Stream.junk toks; let z = atom toks in
                                      let y = timeses toks in
                                      (fun env x -> y env (x * z env))
    | _ -> (* ε *) (fun _ x -> x)

  and timesexp toks =
    match Stream.peek toks with
    | Some (ID x1) -> Stream.junk toks; let x2 = timeses toks in (fun env -> x2 env (List.assoc x1 env))
    | Some (INT x1) -> Stream.junk toks; let x2 = timeses toks in (fun env -> x2 env x1)
    | Some LPAREN -> Stream.junk toks; let x2 = exp toks in
                                       let () = rparen toks in
                                       let x4 = timeses toks in
                                       (fun env -> x4 env (x2 env))
    | _ -> failwith "parsing error"

  and pluses toks =
    match Stream.peek toks with
    | Some PLUS -> Stream.junk toks; let x2 = timesexp toks in let x3 = pluses toks in (fun env x -> x3 env (x + x2 env))
    | Some MINUS -> Stream.junk toks; let x2 = timesexp toks in let x3 = pluses toks in (fun env x -> x3 env (x - x2 env))
    | _ -> (* ε *) (fun _env x -> x)

  and times_plus toks =
    match Stream.peek toks with
    | Some (ID x1) -> Stream.junk toks; let x2 = timeses toks in
                                        let x3 = pluses toks in
                                        (fun env -> x3 env (x2 env (List.assoc x1 env)))
    | Some (INT x1) -> Stream.junk toks; let x2 = timeses toks in
                                         let x3 = pluses toks in
                                         (fun env -> x3 env (x2 env x1))
    | Some LPAREN -> Stream.junk toks; let x2 = exp toks in
                                       let () = rparen toks in
                                       let x4 = timeses toks in
                                       let x5 = pluses toks in
                                       (fun env -> x5 env (x4 env (x2 env)))
    | _ -> failwith "parsing error"

  and equalses toks =
    match Stream.peek toks with
    | Some EQUAL -> Stream.junk toks; let x2 = times_plus toks in
                                      let x3 = equalses toks in
                                      (fun env x -> x3 env (if (x = x2 env) then 1 else 0))
    | _ -> (* ε *) (fun _env x -> x)

  and exp  toks =
    match Stream.peek toks with
    | Some LET -> Stream.junk toks; let x2 = id toks in
                                    let () = equal_ toks in
                                    let x4 = exp toks in
                                    let () = in_ toks in
                                    let x6 = exp toks in
                                    (fun env -> x6 ((x2,x4 env) :: env))
    | Some (ID x1) -> Stream.junk toks; let x2 = timeses toks in
                                        let x3 = pluses toks in
                                        let x4 = equalses toks in
                                        (fun env -> x4 env (x3 env (x2 env (List.assoc x1 env))))
    | Some (INT x1) -> Stream.junk toks; let x2 = timeses toks in
                                         let x3 = pluses toks in
                                         let x4 = equalses toks in
                                         (fun env -> x4 env (x3 env (x2 env x1)))
    | Some IF -> Stream.junk toks; let x2 = exp toks in
                                   let () = then_ toks in
                                   let x4 = exp toks in
                                   let () = else_ toks in
                                   let x6 = exp toks in
                                   (fun env -> if x2 env <> 0 then x4 env else x6 env)
    | Some LPAREN -> Stream.junk toks; let x2 = exp toks in
                                       let () = rparen toks in
                                       let x4 = timeses toks in
                                       let x5 = pluses toks in
                                       let x6 = equalses toks in
                                       (fun env -> x6 env (x5 env (x4 env (x2 env))))
    | _ -> failwith "parsing error"
end
