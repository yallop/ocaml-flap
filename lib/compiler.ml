(*
 * Copyright (c) 2021 Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Compilation of fused deterministic GNF *)
module Make (Term : sig type t [@@deriving ord, show] end) =
struct
  open Normal.Make(Term)
  open Fused.Make(Term)
  type ('a, 'b) fn = ('a code -> index:int code -> len:int code -> string code -> 'b code)

  module Idx =
  struct
    open Env
    type 'a payload = { nt: 'a Var.t;
                        prods: productions;
                        nexts: ((Reex.t * (int ref code -> index:int code -> len:int code -> string code -> 'a code)) list);
                        fn: (int ref, 'a) fn; }
    type 'a t = T : 'a payload -> (int ref -> index:int -> prev:int -> len:int -> string -> 'a) t

    let rec eql_prods : type a b. (Reex.t * a) list -> (Reex.t * b) list -> bool =
      fun l r ->
      match l, r with
      | [], [] -> true
      | [], _ | _, [] -> false
      | (x,_) :: xs, (y,_) :: ys -> Reex.equal x y && eql_prods xs ys

    let eql (type a b) (T {nt=l; nexts=lrhs;_} : a t) (T {nt=r; nexts=rrhs;_} : b t) : (a, b) Letrec.eql option =
      match Var.eql l r with
      | Some Refl when eql_prods lrhs rrhs (* && lf == rf *) -> Some Refl
      | _ -> None
  end
  module Rec = Letrec.Make(Idx)
  module Normalize = Normalize.Make(Term)

  let eps_fallback = function
    | None -> fun _ ~index:_  ~len:_ _ -> .<failwith "unexpected eof" >.
    | Some c -> fun _ ~index:_  ~len:_ _ -> Code.dyn c

  (* The general algorithm is as follows:

       we maintain a mapping M: regex ↦ continuation
       we generate a function f_NT_M for each (NT, M) pair that behaves as follows:
          X := [(c, δc(M)) | c ∈ Σ]
          rnull = r ∈ M such that ε ∈ r
          match on s(i):
           c ↦  if all δc(M) are ⊥ then if rnull continue with M[rnull]
                                         else fail
                 else continue with f_NT_δc(M)
   *)
  let make_cases_exclusive cases =
    (List.rev @@ Stdlib.fst @@ List.fold_left
      (fun (cases, seen) (lhs, rhs) ->
        Reex.(let lhs' = lhs <&> not seen in ((lhs', rhs) :: cases, seen <|> lhs')))
      ([], Reex.empty)
      cases)

  let rec ntseq : type a. Rec.resolve -> productions -> int ref code -> len:int code -> string code -> a ntseq -> a Code.t =
    fun resolve ps start ~len s seq ->
    match seq with
    | Empty -> Code.unit
    | Cons (x, xs) -> let cases, eps = lookup ps x in
                      let cases' = List.map (fun (r, k) -> (r, unCont resolve ps k)) cases in
                      Code.let_ (Code.inj .< .~(resolve.Rec.resolve (Idx.T {nt=x; prods=ps; nexts=cases';
                                                                            fn =eps_fallback eps}))
                                               .~start ~prev:0 ~len:.~len ~index:(! .~start) .~s >.) @@ fun x' ->
                      Code.let_ (ntseq resolve ps start ~len s xs) @@ fun xs' ->
                      Code.pair x' xs'

  and prod : type a. Rec.resolve -> productions -> a Normal.Make(Term).prod -> int ref code -> len:int code -> string code -> string code -> a Code.t
    = fun r ps (Prod {nonterms; semact}) start ~len s tok ->
    Code.let_ (ntseq r ps start ~len s nonterms) @@ fun x ->
    semact (Code.injv tok) x

  and unCont : type a. Rec.resolve -> productions -> a cont -> int ref code -> index:int code -> len:int code -> string code -> a code =
    fun resolve ps k start ~index:i ~len s ->
    match k with
    | Success p -> .< let _start = ! .~start in let _ = .~start := .~i in
                    .~(Code.dyn (prod resolve ps p start ~len s .<String.sub .~s _start (.~i - _start)>.)) >.
    | Error e -> .< Printf.ksprintf failwith "Error (position %d): %s" .~i e >.

  let rhs (type a) {Rec.resolve} (Idx.T {nt; prods; nexts=rhs; fn} : a Idx.t) : a code =
    Reex_match.matchk_ ~options:{match_type=`ranges; null_optimization=true}
      (fun (nexts, fn) -> resolve (T {nt; prods; nexts; fn})) (rhs, fn)

  let compile cases g =
    let {start; items} = fuse (make_cases_exclusive cases) (Normalize.resolve g) in
    Rec.letrec {rhs=rhs}
      (fun {resolve} ->
        .< fun s -> let start' = ref 0 and len = String.length s in
                    .~(let rs, eps = lookup items start in
                       resolve (T {nt=start; prods=items; fn = eps_fallback eps;
                                   nexts=List.map (fun (r, k) -> (r, unCont {resolve} items k)) rs }))
                         start' ~index:0 ~prev:0 ~len s >.)
end
