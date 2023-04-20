(*
 * Copyright (c) 2023 Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

type _ tag =
  | CRLF : unit tag
  | COMMA : unit tag
  | FIELD : char list tag

module Tag =
struct
  type 'a t = 'a tag

  let print : type a. Format.formatter -> a t -> unit =
    fun fmt -> function
     | CRLF  -> Format.fprintf fmt "CRLF"
     | COMMA -> Format.fprintf fmt "COMMA"
     | FIELD -> Format.fprintf fmt "FIELD"

  let compare : type a b.a t -> b t -> (a, b) Asp.Types.cmp =
    fun x y ->
    match x, y with
    | CRLF , CRLF  -> Eql
    | COMMA, COMMA -> Eql
    | FIELD, FIELD -> Eql
    | x    , y   -> (assert (Obj.repr x <> Obj.repr y); if Obj.repr x < Obj.repr y then Leq else Geq)
end

type utag = U : _ tag -> utag [@@unboxed]

type t = T : 'a tag * 'a -> t

let to_int : utag -> int = Obj.magic

let tag : t -> utag = function (T (tag,_)) -> U tag

module Ord =
struct
  type t = utag
  let compare l r = Stdlib.compare (to_int l) (to_int r)
end

module TagSet = Set.Make(Ord)
let all = TagSet.of_list
            [U CRLF;
             U COMMA;
             U FIELD]

let inj t = U t

let match_ : type a b. t -> a tag -> (a -> b) -> (unit -> b) -> b =
  fun (T (tag, v)) tag' yes no ->
  if U tag = U tag' then yes (Obj.magic v) else no ()

let matchlist_ : type a b. t -> a tag list -> (a -> b) -> (unit -> b) -> b =
  fun (T (tag, v)) tags yes no ->
  if List.mem (Obj.magic tag) tags then yes (Obj.magic v) else no ()
