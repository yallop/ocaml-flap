(*
 * Copyright (c) 2023 Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(**
  Representations of fused grammars constructed from separate lexers and parsers.

  This corresponds to the grammars defined in Figure 6 of:

     flap: A Deterministic Parser with Fused Lexing
     PLDI 2023
 *)

module Make(Term: sig type t [@@deriving ord, show] end) :
sig
  type lexer_rhs = Skip | Error of string | Return of Term.t
  (** A lexer right-hand side is an action: either [Skip] (restart lexing),
      [Error e] (fail with an error message) or [Return v] (succeed, returning a value) *)

  type 'a cont = Success of 'a Normal.Make(Term).prod
               | Error of string
  (** The continuation of the regex corresponding to a token.  It is either
       [Error s]
      (i.e. reading the token causes parsing to fail), or
       [Success p]
      where [p] represents the right-hand side of a normalized production.
      The documentation of the [Normal] module gives more details about the [prod] type.
   *)

  type productions
  (** The abstract type of the productions of a fused grammar *)

  val lookup : productions -> 'a Env.Var.t -> (Reex.t * 'a cont) list * 'a Code.t option
  (** [lookup prods nt] returns [(prods, epso)], where [prods] is a list of
      productions for the nonterminal [nt].

      Each production has the form [(r,cont)], where [r] is a regex that reads a token
      and [cont] is the nonterminal sequence that is used to continue parsing after
      the token has been read.

      The second component, [eo], represents the optional epsilon production that is
      used for backtracking if none of the other productions matches the input.
   *)

  type 'a grammar = { start: 'a Env.Var.t; items: productions; } [@@deriving show]
  (** A fused grammar is a start symbol and a set of productions *)

  val fuse : (Reex.t * lexer_rhs) list -> 'a Normal.Make(Term).grammar -> 'a grammar
  (** [fuse l p] constructs a fused grammar from a lexer [l] and a
      parser [p] in Deterministic Greibach Normal Form, as defined in Figure 6 of:

     flap: A Deterministic Parser with Fused Lexing
     PLDI 2023
   *)
end
