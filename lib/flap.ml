(*
 * Copyright (c) 2023 Neel Krishnaswami and Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

module Parse (Term : sig type t [@@deriving ord, show] end) = struct
  module V = Code
  module C =
  struct
    include Set.Make(Term)
    let pp fmt cs = Format.fprintf fmt "{ ";
                    iter (fun _ -> Format.fprintf fmt "???, ") cs;
                    Format.fprintf fmt "}"
  end

  module Tp = struct
    type t = { first : C.t; follow : C.t; null : bool; guarded : bool } [@@deriving show]

    let equal t1 t2 =
         (t1.null = t2.null)
      && (t1.guarded = t2.guarded)
      && C.equal t1.first t2.first
      && C.equal t1.follow t2.follow

    let nonoverlapping cs cs' = C.is_empty (C.inter cs cs')
    let (++) = C.union
    let (==>) b cs = if b then cs else C.empty

    let apart t1 t2 = not t1.null && (nonoverlapping t1.follow t2.first)
    let disjoint t1 t2 = not (t1.null && t2.null) && nonoverlapping t1.first t2.first

    let eps = { first = C.empty; follow = C.empty; null = true; guarded = true }
    let tok c = { first = C.singleton c; follow = C.empty; null = false; guarded = true }
    let seq t1 t2 = if apart t1 t2 then {
          first = t1.first;
          follow = t2.follow ++ (t2.null ==> t2.first ++ t1.follow);
          null = false;
          guarded = t1.guarded
        } else Format.kasprintf failwith "seq %a %a" pp t1 pp t2
    let bot = { first = C.empty; follow = C.empty; null = false; guarded = true }
    let alt t1 t2 = if disjoint t1 t2 then {
          first = t1.first ++ t2.first;
          follow = t1.follow ++ t2.follow;
          null = t1.null || t2.null;
          guarded = t1.guarded && t2.guarded;
        } else Format.kasprintf failwith "alt %a %a" pp t1 pp t2

    let star t = {(seq t t) with null = true; follow = t.first ++ t.follow}

    let min = { first = C.empty; follow = C.empty; null = false; guarded = false }

    let fix f =
      let rec loop tp =
        let tp' = f tp in
        if equal tp tp' then
          tp
        else
          loop tp'
      in
      loop min
  end

  module Var = struct
    type ('ctx, 'a) t =
      | Z : ('a * 'rest, 'a) t
      | S : ('rest, 'a) t -> ('b * 'rest, 'a) t
  end

  module Env(T : sig type 'a t end) = struct
    type 'ctx t =
      | [] : unit t
      | (::) : 'a T.t * 'ctx t -> ('a * 'ctx) t

    let rec lookup : type ctx a. ctx t -> (ctx, a) Var.t -> a T.t =
      fun env n ->
      match n, env with
      | Z, x :: _   -> x
      | S n, _ :: xs -> lookup xs n

    type fn = {f : 'a. 'a T.t -> 'a T.t}
    let rec map : type ctx. fn -> ctx t -> ctx t = 
      fun {f} ctx -> 
      match ctx with 
      | [] -> [] 
      | x :: xs -> f x :: map {f} xs                                   
  end

  type 'a tag = Term.t * (string V.t -> 'a V.t)

  module Grammar = struct
    type ('ctx, 'a) var = ('ctx, 'a) Var.t =
      | Z : ('a * 'rest, 'a) var
      | S : ('rest, 'a) var -> ('b * 'rest, 'a) var

    type ('ctx, 'a, 'd) t' =
      Eps : 'a V.t -> ('ctx, 'a, 'd) t'
    | Seq : ('ctx, 'a, 'd) t * ('ctx, 'b, 'd) t -> ('ctx, 'a * 'b, 'd) t'
    | Tok : 'a tag -> ('ctx, 'a, 'd) t'
    | Bot : ('ctx, 'a, 'd) t'
    | Alt : ('ctx, 'a, 'd) t * ('ctx, 'a, 'd) t -> ('ctx, 'a, 'd) t'
    | Map : ('a V.t -> 'b V.t) * ('ctx, 'a, 'd) t -> ('ctx, 'b, 'd) t'
    | Fix : ('a * 'ctx, 'a, 'd) t -> ('ctx, 'a, 'd) t'
    | Var : ('ctx,'a) var -> ('ctx, 'a, 'd) t'
    | Star : ('ctx, 'a, 'd) t -> ('ctx, 'a list, 'd) t'
    and ('ctx, 'a, 'd) t = 'd * ('ctx, 'a, 'd) t'

    module TpEnv = Env(struct type 'a t = Tp.t end)

    let data : type ctx a d. (ctx, a, d) t -> d = fun (d, _) -> d

    let rec typeof : type ctx a d. ctx TpEnv.t -> (ctx, a, d) t -> (ctx, a, Tp.t) t  =
      fun env (_, g) ->
      match g with
      | Eps v -> (Tp.eps, Eps v)
      | Seq (g1, g2) -> let guarded tp = {tp with Tp.guarded = true} in 
                        let g1 = typeof env g1 in
                        let g2 = typeof (TpEnv.map {TpEnv.f = guarded} env) g2 in
                        (Tp.seq (data g1) (data g2), Seq(g1, g2))
      | Tok (tag,f) -> (Tp.tok tag, Tok (tag,f))
      | Bot  -> (Tp.bot, Bot)
      | Alt (g1, g2) -> let g1 = typeof env g1 in
                        let g2 = typeof env g2 in
                        (Tp.alt (data g1) (data g2), Alt(g1, g2))
      | Map(f, g') -> let g' = typeof env g' in
                      (data g', Map(f, g'))
      | Star g -> let g = typeof env g in
                  (Tp.star (data g), Star g)
      | Fix g'  -> let tp = Tp.fix (fun tp -> data (typeof (tp :: env) g')) in
                   if tp.guarded then let g' = typeof (tp :: env) g' in (data g', Fix g')
                   else Format.kasprintf failwith "guarded %a" Tp.pp tp
      | Var n -> (TpEnv.lookup env n, Var n)
  end

  module HOAS = struct
    (* Higher-order abstract syntax front-end to the parser
     combinators *)
    open Grammar

    module Ctx = Env(struct type 'a t = unit end)

    let rec len : type n. n Ctx.t -> int = fun ctx ->
      match ctx with
      | [] -> 0
      | () :: ctx -> 1 + len ctx

    let rec tshift' : type a i j. int -> j Ctx.t -> (a * i) Ctx.t -> (j, a) Var.t =
      fun n c1 c2 ->
      match n, c1, c2 with
        (* Both the 'assert false' and the 'Obj.magic' are safe here,
           since we know (although it's not captured in the types) that
           (a * i) is a prefix of j.

           More details: "Unembedding Domain Specific Languages" §4.4.
         *)
      |        _, [], _ -> assert false
      | 0, () :: _, () :: _ -> Obj.magic Var.Z
      | n, () :: c1, c2 -> Var.S (tshift' (n-1) c1 c2)

    let tshift : type a i j. j Ctx.t -> (a * i) Ctx.t -> (j, a) Var.t =
      fun c1 c2 -> tshift' (len c1 - len c2) c1 c2

    type 'a t = { untdb : 'ctx. 'ctx Ctx.t -> ('ctx, 'a, unit) Grammar.t }

    let eps a = { untdb = fun _ -> (), Eps a }
    let (>>>) f g = { untdb = fun i -> (), Seq (f.untdb i, g.untdb i) }
    let tok t f = { untdb = fun _ -> (), Tok (t, f) }
    let bot = { untdb = fun _ -> (), Bot }
    let (<|>) f g = { untdb = fun i -> (), Alt (f.untdb i, g.untdb i) }
    let any gs = List.fold_left (<|>) bot gs
    let ($) g f = { untdb = fun i -> (), Map (f, g.untdb i) }
    let fix f =
      { untdb = fun i ->
                (), Fix ((f {untdb = fun j -> (), Var (tshift j (() :: i))}).untdb
                       (() :: i)) }
    let star g = { untdb = fun i -> (), Star (g.untdb i) }
  end
  include HOAS

  module Normalize =
  struct
    module G = Normalize.Make(Term)

    module Ctx = Env(G)

    let rec norm : type a ctx. ctx Ctx.t -> (ctx, a, Tp.t) Grammar.t -> a G.t =
      fun ctx -> function
      | _, Eps v -> G.eps v
      | _, Seq (g, h) -> G.(norm ctx g >>> norm ctx h)
      | _, Tok (t, f) -> G.(tok (t $$ f))
      | _, Bot -> G.bot ()
      | _, Alt (g, h) -> G.(norm ctx g <|> norm ctx h)
      | _, Map (f, g) -> G.(norm ctx g $ f)
      | t, Fix body -> G.fix t.first (fun h -> norm (h :: ctx) body)
      | _, Var x -> Ctx.lookup ctx x
      | _, Star _ -> failwith "star normalization not yet implemented"
  end

  module Compiler = Compiler.Make(Term)
  type rhs = Fused.Make(Term).lexer_rhs = Skip | Error of string | Return of Term.t
  let type_check {untdb} = Grammar.typeof [] (untdb [])
  let compile l p = match type_check p with
    | p -> Ok (Compiler.compile l (Normalize.norm [] p))
    | exception (Failure e) -> Error e
end
module Cd = Code

