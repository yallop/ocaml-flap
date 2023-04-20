(*
 * Copyright (c) 2021 Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

module NT = Env.Var
let pr = Format.fprintf

module Unresolved(Term: sig [@@@ocaml.warning "-32"] type t [@@deriving ord, show] end) =
struct
  (** Normal form grammars with unresolved references *)

  module TermMap = Map.Make(Term)
  module Terms = Set.Make(Term)

  (** prod ::= {T} NT* ↦ S[e] *)
  type 'l prod = Prod : { nonterms: 'n ntseq; semact: string Code.t -> 'n Code.t -> 'l Code.t } -> 'l prod
               | Prod_of of Term.t * 'l NT.t
               | Map : ('m Code.t -> 'l Code.t) * 'm prod -> 'l prod
  and 'a ntseq =
    Empty : unit ntseq
  | One of 'a NT.t
  | Nonterms_of of Term.t * 'a NT.t
  | Append : 'a ntseq * 'b ntseq -> ('a * 'b) ntseq 
  type 'a null =
    Some of 'a Code.t
  | None
  | Null_of of 'a NT.t
  | MapNull : ('a Code.t -> 'b Code.t) * 'a null -> 'b null
  | Either of 'a null * 'a null
  type 'a rhs = { prods: 'a prod TermMap.t; null: 'a null; (* Do we accept the empty string? *) }

  module Productions = Env.Env(struct type 'a t = 'a rhs end)

  type 'a grammar = {
    start: 'a NT.t;
    items: Productions.t;
  }
  type 'a t = 'a grammar

  let rec pp_prod : type a. Format.formatter -> a prod -> unit =
    fun fmt -> function
    | Prod p -> pp_ntseq fmt p.nonterms
    | Prod_of (t, x) -> pr fmt "(Prod_of %a %s)" Term.pp t (NT.str x)
    | Map (_, p) -> pr fmt "(Map <fun> %a)" pp_prod p
  and pp_ntseq : type a. Format.formatter -> a ntseq -> unit =
    fun fmt -> function
    | Empty -> ()
    | One x -> pr fmt "%s" (NT.str x)
    | Nonterms_of (t, x) -> pr fmt "(Nonterms_of %a %s)" Term.pp t (NT.str x)
    | Append (x, y) -> pr fmt "@[%a@ %a@]" pp_ntseq x pp_ntseq y

  let rec pp_null : type a. Format.formatter -> a null -> unit =
    fun fmt -> function
    | Some _ -> pr fmt "Some"
    | None   -> pr fmt "None"
    | Null_of x -> pr fmt "(Null_of %s)" (NT.str x)
    | MapNull (_, n) -> pp_null fmt n
    | Either (l, r) -> pr fmt "(Either %a %a)" pp_null l pp_null r

  let pp_rhs : type a. Format.formatter -> a rhs -> unit =
    fun fmt r ->
    begin
      TermMap.iter (fun t p -> pr fmt " |@ %a @[%a@]" Term.pp t pp_prod p) r.prods;
      pr fmt "|@ ";
      pp_null fmt r.null 
    end

  let pp_productions fmt p =
    Productions.pp (object method m : 'a. Format.formatter -> 'a rhs -> unit = pp_rhs end) fmt p

  let pp_grammar fmt g = pr fmt "<%s,@ @[%a@]>" (NT.str g.start) pp_productions g.items
  let pp _ = pp_grammar
  let show _ = Format.asprintf "%a" (pp ())

  type 'a tok = Term.t * (string Code.t -> 'a Code.t)
  let ($$) t f = (t, f)

  open Productions
  let fresh k = let start = NT.var () in { start; items = k start }

  let eps v = fresh @@ fun s -> bind s {prods=TermMap.empty; null=Some v} empty

  let seqprod a t s = Prod { nonterms = Append (Nonterms_of (t, a), One s);
                             semact = (fun _ n -> n) }

  let ( >>> ) g g' = fresh @@ fun start ->
    let gstarts = g.items.%{g.start} in
    bind start {prods=TermMap.mapi (fun t _ -> seqprod g.start t g'.start) gstarts.prods; null=None} (extend g.items g'.items)

  let tok (t,f) = fresh @@ fun start ->
              bind start {prods=TermMap.singleton t (Prod {nonterms = Empty; semact = (fun c _ -> f c)});
                          null=None} empty

  let bot () = fresh @@ fun s -> bind s {prods=TermMap.empty; null=None} empty

  let ( <|> ) g g' =
    fresh @@ fun start -> (* guaranteed first sets distinct *)
    bind start {prods=TermMap.union (fun _ _ _ -> assert false) g.items.%{g.start}.prods g'.items.%{g'.start}.prods ;
                null = Either (g.items.%{g.start}.null, g'.items.%{g'.start}.null) } (extend g.items g'.items)

  let ( $ ) { start ; items } f = fresh @@ fun start' ->
    let startprods = items.%{start} in
    let prods = TermMap.map (fun p -> Map (f, p)) startprods.prods in
    let null = MapNull (f, startprods.null) in
    bind start' {prods; null} items

  let fix ts f =
    (* We've arranged things above so that nothing actually accesses the entries for a particular
       grammar; instead, it inserts a reference (e.g. 'Prod_of') to resolve later.
       (There may be some subtleties here: does the semact end up as a recursive function?) *)
    let start = NT.var () in
    let prods = Terms.fold (fun t m -> TermMap.add t (Prod_of (t, start)) m) ts TermMap.empty in
    let dummy = { start; items = bind start {prods; null = Null_of start} empty } in
    let g = f dummy in
    {g with items = alias start g.start g.items}
end

module Make(Term: sig type t [@@deriving ord, show] end) =
struct
  module N = Normal.Make(Term)
  module U = Unresolved(Term)

  (** Translation to eliminate unresolved references, producing fully normalized grammars *)

  let rec prod : type l. U.Productions.t -> l U.prod -> l N.prod =
    fun prods -> function
    | Prod {nonterms=Empty; semact} ->
       N.Prod {nonterms=Empty; semact}
    | Prod {nonterms=One x; semact} ->
       N.Prod {nonterms=Cons (U.Productions.canonicalize x prods, Empty); semact = fun t n -> semact t (Code.fst n)}
    | Prod {nonterms=Nonterms_of (t, x); semact} ->
       let N.Prod {nonterms; semact=sa2} = prod prods (U.TermMap.find t U.Productions.(prods.%{x}.prods)) in
       N.Prod {nonterms; semact=fun t n -> semact t (sa2 t n)}
    | Prod {nonterms=Append (l, r); semact=sa} ->
       let N.Prod {nonterms=ntl; semact=sal} = prod prods (Prod {nonterms=l;semact=(fun _ n -> n)}) in
       let N.Prod {nonterms=ntr; semact=sar} = prod prods (Prod {nonterms=r;semact=(fun _ n -> n)}) in
       append ntl sal ntr sar sa
    | Prod_of (t, x) -> prod prods U.Productions.(U.TermMap.find t prods.%{x}.prods)
    | Map (f, p) -> let N.Prod {nonterms; semact} = prod prods p in
                    N.Prod {nonterms; semact = fun t n -> f (semact t n)}
  and append : type a b c d e. a N.ntseq -> (string Code.t -> a Code.t -> b Code.t) ->
                               c N.ntseq -> (string Code.t -> c Code.t -> d Code.t) ->
                               (string Code.t -> (b * d) Code.t -> e Code.t) ->
                               e N.prod =
    fun ntl sal ntr sar sa ->
    match ntl with
    | Empty -> N.Prod { nonterms = ntr;
                        semact = fun t n -> let l = sal t Code.unit in
                                            let r = sar t n in
                                            sa t (Code.pair l r) } 
    | Cons (x, xs) ->
       let N.Prod { nonterms = xs'; semact = semact' } = append xs (fun _ n -> n) ntr sar (fun _ ns -> ns) in
       Prod { nonterms = Cons (x, xs');
              semact = fun t n ->
                       Code.let_ n @@ fun n ->
                       Code.let_ (semact' t (Code.snd n)) @@ fun s ->
                       sa t (Code.pair (sal t (Code.pair (Code.fst n) (Code.fst s))) (Code.snd s)) }
  and rhs : type a. U.Productions.t -> a U.rhs -> a N.rhs =
    fun prods { prods=p; null=n } -> { prods = U.TermMap.map (prod prods) p; null = null prods n}
  and null : type a. U.Productions.t -> a U.null -> a Code.t option = fun prods -> function
    | Some v -> Some v
    | None -> None
    | Null_of x -> null prods U.Productions.(prods.%{x}.null)
    | MapNull (f, v) -> Option.map f (null prods v)
    | Either (x, y) -> match null prods x, null prods y with (* guaranteed not both null *)
                       | None, None -> None
                       | Some x, _ -> Some x
                       | _, Some y -> Some y

  let grammar : type a. a U.grammar -> a N.grammar =
    fun { start; items } ->
    { start;
      items = List.fold_right (fun p e -> match p with (U.Productions.E (x, r)) -> N.Productions.bind x (rhs items r) e
                                                     | (U.Productions.Alias (x, y)) -> N.Productions.alias x y e)
                (items :> U.Productions.entry list)
                N.Productions.empty }
  include U
  let resolve = grammar
end
