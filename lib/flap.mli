(*
 * Copyright (c) 2023 Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

module Parse (Term: sig type t [@@deriving ord, show] end) :
sig
  type _ t
  (** The type of parsers *)

  val eps : 'a Code.t -> 'a t
  (** [eps v] succeeds without consuming input and returns [v] *)

  val ( >>> ) : 'a t -> 'b t -> ('a * 'b) t
  (** [p >>> q] parses successive prefixes of the input using [p] and then [q]
      and returns a pair of the result of the two parses *)

  val tok : Term.t -> (string Code.t -> 'a Code.t) -> 'a t
  (** [term t] parses a token whose tag is [t], or fails *)

  val bot : 'a t
  (** [bot] fails without consuming input *)

  val ( <|> ) : 'a t -> 'a t -> 'a t
  (** [p <|> q] parses using either [p] or [q], which must be disjoint
     --- that is, they must not both accept the empty string, and
     their first sets must not overlap *)

  val any : 'a t list -> 'a t
  (** [any [p1; p2; ... pn]] is [p1 <|> p2 <|> ... <|> pn]  *)

  val ( $ ) : 'a t -> ('a Code.t -> 'b Code.t) -> 'b t
  (** If [p] parses the input and returns [v] then
      [p $ f] parses the input and returns [f v] *)

  val fix : ('b t -> 'b t) -> 'b t
  (** [fix (fun x -> e)] creates a recursive parser [e].  In the body [e]
      the parser can be accessed using the bound variable [x]. *)

  val star : 'a t -> 'a list t
  (** [star p] repeatedly parses successive prefixes of the input using [p],
      and returns a list of the zero or more results *)

  type rhs = Skip | Error of string | Return of Term.t
  val compile : (Reex.t * rhs) list -> 'a t -> ((string -> 'a) code, string) result
  (** [compile l p] builds code [Ok c] for a lexer [l] and type-checked parser [p],
      or fails with an error message [Error e] if the parser does not satisfy the
      typing constraints (e.g. if the grammar is ambiguous or contains left-recursion) *)
end

module Cd : module type of Code

