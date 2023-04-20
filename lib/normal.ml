let pr = Format.fprintf

(** Greibach Normal Form grammars. *)

(** A grammar is in Greibach Normal Form (GNF) if all RHSs consist of
   a terminal followed by a sequence of nonterminals.  The types in
   this module can only represent grammars in a variation of GNF.

   Our grammars differ from GNF in two ways:
   (1) we allow all productions to include guarded ε transitions.
       They're "guarded" in the sense that they're only active when
       none of the terminals in the other productions match the input. 
   (2) the terminals in the leftmost position of each rhs must be
       disjoint for a given symbol: the grammars are "deterministic". 

   Why not push epsilon transitions up to the top (start symbol)?  Two reasons:
   (1) it's hard to do compositionally: de-ε-ing (g . (ε + g')) requires looking
       at the _use_ sites of the expression.
   (2) standard ε elimination makes grammars non-deterministic, which we need to avoid.
 *)

module Make (Term : sig [@@@ocaml.warning "-32"] type t [@@deriving ord, show] end) =
struct
  module TermMap = Map.Make(Term)

  type 'a ntseq = Empty : unit ntseq
                | Cons : 'a Env.Var.t * 'b ntseq -> ('a * 'b) ntseq
  let rec ntseq : type a. Format.formatter -> a ntseq -> unit =
    fun fmt -> function
            | Empty -> ()
            | Cons (x, xs) -> pr fmt "@[%s@ %a@]" (Env.Var.str x) ntseq xs

  type 'l prod = Prod : { nonterms: 'n ntseq; semact: string Code.t -> 'n Code.t -> 'l Code.t } -> 'l prod
  let pp_prod : type a. _ -> Format.formatter -> a prod -> unit =
    fun _ fmt (Prod p) -> ntseq fmt p.nonterms
  let show_prod _ = Format.asprintf "%a" (pp_prod ())

  type 'a rhs = { prods: 'a prod TermMap.t; null: 'a Code.t option }
  let pp_rhs : type a. _ -> Format.formatter -> a rhs -> unit =
    fun _ fmt r ->
    begin
      TermMap.iter (fun t p -> pr fmt " |@ %a @[%a@]" Term.pp t (pp_prod ()) p) r.prods;
      pr fmt "|@ ";
      if Option.is_some r.null then pr fmt "| ε";
    end
  let show_rhs _ = Format.asprintf "%a" (pp_rhs ())

  module Productions = Env.Env(struct type 'a t = 'a rhs end)
  let productions fmt p =
    Productions.pp (object method m : 'a. Format.formatter -> 'a rhs -> unit = pp_rhs () end) fmt p

  type 'a grammar = { start: 'a Env.Var.t; items: Productions.t; }
  let pp_grammar _ fmt g = pr fmt "<%s,@ @[%a@]>" (Env.Var.str g.start) productions g.items
  let show_grammar _ = Format.asprintf "%a" (pp_grammar ())

  type var = Var : _ Env.Var.t -> var 

  let var_eql : var -> var -> bool =
    fun (Var x) (Var y) ->
    match Env.Var.eql x y with
    | Some _ -> true
    | None -> false

  let var_mem : var -> var list -> bool =
    fun x -> List.exists (var_eql x)

  let trim : type a. a grammar -> a grammar =
    fun { start; items } ->
    let rec add_nonterms : type a. a ntseq -> seen:var list -> var list -> var list  =
      fun nts ~seen todo ->
      match nts with
      | Empty -> todo
      | Cons (x, xs) when var_mem (Var x) seen -> add_nonterms xs ~seen todo
      | Cons (x, xs) when var_mem (Var x) todo -> add_nonterms xs ~seen todo
      | Cons (x, xs) -> add_nonterms xs ~seen (Var x :: todo)
    in
    let do_one (Var sym) ~seen todo =
      TermMap.fold
        (fun _ (Prod {nonterms;_}) todo -> add_nonterms nonterms ~seen todo)
        Productions.(items.%{sym}).prods
        todo
    in
    let rec loop seen (todo : var list) =
      match todo with
      | [] -> seen
      | x :: xs -> let seen = x :: seen in
                   let todo = do_one x ~seen:(x :: seen) xs in
                   loop seen todo
    in
    let seen = loop [] [Var start] in
    let prods = 
      List.fold_left
        (fun t -> function
          | Productions.E (x,rhs) -> if var_mem (Var x) seen
                                     then Productions.bind x rhs t
                                     else t
          | Alias _ -> t)
        Productions.empty
        (items :> Productions.entry list)
    in
    { start; items = prods }
end
