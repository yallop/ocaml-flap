(*
 * Copyright (c) 2021 Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

module NT = Env.Var

module V = Code
module Make(Term: sig type t [@@deriving ord, show] end) =
struct
  module Gnf = Normal.Make(Term)
  module TermMap = Map.Make(Term)
  module ReexMap = Map.Make(Reex)
  type 'a cont = Success of 'a Gnf.prod
               | Error of string
  module Productions = Env.Env(struct type 'a t = (Reex.t * 'a cont) list * 'a V.t option end)
  type productions = Productions.t
  type 'a grammar = { start: 'a NT.t; items: Productions.t; }

  let lookup = Productions.(.%{})

  module PP =
  struct
    let pr = Format.fprintf

    let cont : type a. Format.formatter -> a cont -> unit =
      fun fmt -> function
      | Success p -> pr fmt "✓ %a" (Gnf.pp_prod (fun _ _ -> ())) p
      | Error e -> pr fmt "X %S" e

    let rhs : type a. Format.formatter -> (Reex.t * a cont) list * a V.t option -> unit =
      fun fmt (l, o) ->
      List.iter (fun (r, k) -> pr fmt "@ @[| \"%a\" @[%a@]@]" Reex.pp r cont k) l;
      Option.iter (fun _ -> pr fmt "@ @[| EPS@]") o

    let productions fmt p =
      Productions.pp (object method m : 'a. Format.formatter -> (Reex.t * 'a cont) list * 'a V.t option -> unit = rhs end) fmt p

    let grammar fmt g = pr fmt "<@[%s,@ @[%a@]@]>" (NT.str g.start) productions g.items
  end
  let pp_grammar _ = PP.grammar
  let show_grammar _ = Format.asprintf "%a" PP.grammar

  type lexer_rhs = Skip | Error of string | Return of Term.t
  type inverted_lexer = { skip: Reex.t option;
                          errors: string ReexMap.t;
                          matches: Reex.t TermMap.t }

  let invert_lexer : (Reex.t * lexer_rhs) list -> inverted_lexer =
    let add_rule ilexer (r, rhs) =
      match ilexer, rhs with
      | { skip = None; _ }   , Skip -> { ilexer with skip = Some r }
      | { skip = Some s; _ } , Skip -> { ilexer with skip = Some Reex.(r <|> s) }
      | { errors; _ }        , Error s -> { ilexer with errors = ReexMap.add r s errors }
      | { matches; _ }       , Return t ->
         let matches = match TermMap.find t matches with
         | exception Not_found -> TermMap.add t       r        matches
         | s                   -> TermMap.add t Reex.(r <|> s) matches
         in { ilexer with matches }
    in
    List.fold_left add_rule
      { skip = None; errors = ReexMap.empty; matches = TermMap.empty }

  let rhs : type a. inverted_lexer -> a NT.t -> a Gnf.rhs -> (Reex.t * a cont) list * a V.t option =
    fun { skip; errors; matches } x { prods; null } ->
    (* TODO: check that all the tokens in prods appear in ilexer? *)
    let prods =
      TermMap.fold
        (fun t r rmap ->
          match TermMap.find_opt t prods with
          | Some p -> (r, Success p) :: rmap
          | None -> rmap)
        matches
        []
    in
    let prods =
      match skip with
      | None -> prods
      | Some s -> (s, Success (Gnf.Prod { nonterms = Cons (x, Empty);
                                          semact = fun _ n -> V.fst n })) :: prods
    in
    let prods =
      ReexMap.fold (fun r e l -> (r, (Error e : _ cont)) :: l) errors prods
    in
    prods, null

  let fusei : type a. inverted_lexer -> a Gnf.grammar -> a grammar =
    fun ilexer { start; items } -> (* (Assumption: the regexes are disjoint) *)
    let module P = Gnf.Productions in
    { start; items = List.fold_right
                       (fun p e ->
                         match p with P.E (x, r) -> Productions.bind x (rhs ilexer x r) e
                                    | P.Alias (x, y) -> Productions.alias x y e)
                       (items :> P.entry list)
                       Productions.empty }

  let fuse l g = fusei (invert_lexer l) g
end
