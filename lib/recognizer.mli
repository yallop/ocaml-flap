(*
 * Copyright (c) 2023 Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(**
  This corresponds to the recognizer algorithms defined in Figures 7-9 of

      flap: A Deterministic Parser with Fused Lexing
      PLDI 2023
 *)

module Make(Term : sig type t [@@deriving ord, show] end) :
sig
  module Lexer :
  sig
    val lex : (Reex.t * Fused.Make(Term).lexer_rhs) list -> string -> Term.t list option
   (** [lex] is an implementation of the lexing recognizer algorithm from Figure 7 of

         flap: A Deterministic Parser with Fused Lexing
         PLDI 2023 *)
  end

  module Normalized :
  sig
    val recognize : _ Normal.Make(Term).grammar -> Term.t list -> bool
   (** [recognize] is an implementation of the parsing recognizer algorithm for grammars
       in Deterministic Greibach Normal Form from Figure 8 of

         flap: A Deterministic Parser with Fused Lexing
         PLDI 2023 *)
  end

  module Fused :
  sig
    val recognize : _ Fused.Make(Term).grammar -> string -> bool
   (** [recognize] is an implementation of the parsing recognizer algorithm for fused grammars
       from Figure 9 of

         flap: A Deterministic Parser with Fused Lexing
         PLDI 2023 *)
  end
end
