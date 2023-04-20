(*
 * Copyright (c) 2023 Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

include Csv_tokens_base

let within_ : utag -> utag -> t code -> bool code =
  fun l h x ->
  let l = Csv_tokens_base.to_int l and h = Csv_tokens_base.to_int h in
  if l = h then .< Csv_tokens_base.to_int (Csv_tokens_base.tag .~x) = l >.
  else  .< l <= Csv_tokens_base.to_int (Csv_tokens_base.tag .~x) && Csv_tokens_base.to_int (Csv_tokens_base.tag .~x) <= h >.

let pp_utag fmt (U x) = Tag.print fmt x

let lift_tag : type a. a tag -> a tag code = function
  | CRLF  -> .< Csv_tokens_base.CRLF >.
  | COMMA -> .< Csv_tokens_base.COMMA >.
  | FIELD -> .< Csv_tokens_base.FIELD >.

let rec lift_list f = function
  | [] -> .< [] >.
  | x :: xs -> .< .~(f x) :: .~(lift_list f xs) >.

let test_tag : type a b. complete:bool -> a tag list -> t code ->
                    (a code option -> b code) ->
                    b code =
  fun ~complete tags x k ->
  if complete then k (Some .< snd (Obj.magic .~x) >.) else
  match tags with
    [tag] ->
 .< Csv_tokens_base.match_ .~x .~(lift_tag tag)
                    (fun x -> .~(k (Some .<x>.)))
                    (fun () -> .~(k None)) >.
  | tags ->
 .< Csv_tokens_base.matchlist_ .~x .~(lift_list lift_tag tags)
                    (fun x -> .~(k (Some .<x>.)))
                    (fun () -> .~(k None)) >.
