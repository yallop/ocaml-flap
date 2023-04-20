(*
 * Copyright (c) 2023 Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(**
  Typed context-free expression combinators for constructing grammars
  in Deterministic Greibach Normal Form.

  For details of typed context-free expressions, see:

     A typed, algebraic approach to parsing
     PLDI 2019
     Neelakantan R. Krishnaswami and Jeremy Yallop

  For details of Deterministic Greibach Normal form, see:

     flap: A Deterministic Parser with Fused Lexing
     PLDI 2023
 *)
module Make (Term : sig type t [@@deriving ord, show] end) :
sig
  type 'a t [@@deriving show]
  (** The type of normalized grammars under construction.
      The [resolve] function converts these under-construction grammars
      into a fully normalized form. *)
  
  type 'a tok
  (** The type of tokens *)

  val ( $$ ) : Term.t -> (string Code.t -> 'a Code.t) -> 'a tok
  (** [t $$ f] builds a token from a terminal [t] and a function
      [f] that converts the string corresponding to the token
      during parsing into a value.  For example, if [INT] is a terminal
      for integers, then 

         [INT $$ (fun x -> injv .< int_of_string .~(dyn x) >.)]

      builds a token that will convert strings representing integers
      into OCaml integers during parsing *)

  val eps : 'a Code.t -> 'a t
  (** [eps v] is a normalized grammar that parses the empty string and
     returns [v] *)

  val ( >>> ) : 'a t -> 'b t -> ('a * 'b) t
  (** [g1 >>> g2] is a normalized grammar that parses [uv] and returns
     [(a,b)] if [g1] parses [u] and returns [a] and [g2] parses [v]
     and returns [b]. *)

  val tok : 'a tok -> 'a t
  (** [tok t] is a normalized grammar that parses a single token [t]
      and returns the value associated with the token. *)

  val bot : unit -> 'a t
  (** [bot ()] is a normalized grammar that always fails. *)

  val ( <|> ) : 'a t -> 'a t -> 'a t
  (** [g1 <|> g2] is a normalized grammar that parses either [u] and
      returns [a] or parses [v] and returns [b] if [g1] parses [u] and
      returns [a] and [g2] parses [v] and returns [b]. *)

  val ( $ ) : 'a t -> ('a Code.t -> 'b Code.t) -> 'b t
  (** [g $ f] is a normalized grammar that parses [u] and
      returns [f a] if [g] parses [u] and returns [a]. *)

  val fix : Set.Make(Term).t -> ('b t -> 'b t) -> 'b t
  (** [fix first f] builds a recursive grammar defined as the fixpoint of [f].
      The argument [first] represents the first set of [f]. *)

  val resolve : 'a t -> 'a Normal.Make(Term).grammar
  (** [resolve g] converts a grammar representation [g] into a
      fully-normalized grammar. *)
end
