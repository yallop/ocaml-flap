(*
 * Copyright (c) 2023 Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(** Typed variables and environments. *)

module Var :
sig
  type 'a t
  (** A value of type [s t] is a variable that can be bound to a value
      of type [s f] for some [f] determined by the environment.

      For example, variables of type [int t] and [string t] can be
      bound respectively to values of type [int list] and [string list]
      in an environment that sets [f] to [list].  *)

  val var : unit -> _ t
  (** Construct a fresh variable of type [s t], where [s] is determined by the caller:

     let x : int t = var ()
     let y : string t = var ()
   *)

  val str : _ t -> string
  (** Return a string representation of a variable.  Distinct variables
      have distinct string representations.
   *)

  val eql : 'a t -> 'b t -> ('a, 'b) Letrec.eql option
(** Test two variables [x] and [y] of types [a t] and [b t] for
    equality.

    Return [None] if [x] and [y] are unequal.

    Return [Some Refl] if [x] and [y] are equal; [Refl] serves as a
    proof that the types [a] and [b] are equal.  *)
end

module Env (V: sig type 'a t end) :
sig
  (** Environments parameterised by the (parameterised) type [V.t] of arguments.

      For example, the module [Env(List)] represents environments mapping typed
      variables to lists of values, and the module [Env(Code)] represents
      environments mapping typed variables to code representations. *)

  type entry = E : 'a Var.t * 'a V.t -> entry | Alias : 'a Var.t * 'a Var.t -> entry
  (** An entry in an environment: either [E (x,v)], which associates variable
      [x] with value [v], or [A (x,y)], which records that [x] is an alias for [y].
      Resolving a variable [x] in an environment involves resolving aliases
      until an entry is found.
   *)

  type t = private entry list
  (** The type of environments *) 

  val empty : t
  (** The empty environment *) 

  val bind : 'a Var.t -> 'a V.t -> t -> t
  (** [bind x v env] builds a new environment containing everything in [env]
      and an additional binding mapping [x] to [v], leaving [env] unchanged.
      During lookup, the new binding for [x] has priority over existing bindings.
 *) 

  val alias : 'a Var.t -> 'a Var.t -> t -> t
  (** [alias x y env] builds a new environment containing everything in [env]
      and an additional alias associating [x] with [y], leaving [env] unchanged.
      During resolution, the new alias for [x] has priority over existing aliases.
 *) 

  val canonicalize : 'a Var.t -> t -> 'a Var.t

  (** [canonicalize x env] returns the result of resolving the alias
      chain for variable [x] in environment [env].

      For example, if [env] contains entries [Alias (x,y)] and [Alias (y,z)],
      but no aliases [Alias (z,w)] for any [w], then [canonicalize x env]
      returns [z]. *) 

  val extend : t -> t -> t
  (** [extend lenv renv] adds all the entries of [lenv] to [renv]. *) 

  val (.%{}) : t -> 'a Var.t -> 'a V.t
  (** [env.%{x}] looks up variable [x] in environment [env] *)

  val pp : <m:'a.Format.formatter -> 'a V.t -> unit> -> Format.formatter -> t -> unit
  (** [pp] is a pretty-printer for environments.  The first argument 
      is a polymorphic pretty-printer for values *)
end
