(* A recognizer for fused grammars *)

module Make(Term : sig type t [@@deriving ord, show] end) =
struct
  module N = Normal.Make(Term)
  module R = Reex_match__.Reex_deriv

  let deriv_rhss c = List.map (fun (r, k) -> Reex_match__.Reex_deriv.deriv c r, k)

  let without_unmatchables rhss = (* Remove rhss with ⊥ regexes *)
    List.filter (fun (r,_) -> not Reex.(equal r empty)) rhss

  let first_nullable rs = List.find_opt (fun (r,_) -> R.nullable r) rs

  let explode (s : string) : char list = List.init (String.length s) (String.get s)

  let map_null s = Option.map (fun _ -> s)

  module Lexer =
  struct
    module F = Fused.Make(Term)
    type result = Term of Term.t | Skip
    type last = (result * char list) option

    let last_of_rhs s = function
      | (_,F.Skip)   -> Some (Skip,s)
      | (_,Error _)  -> None
      | (_,Return t) -> Some (Term t,s)

    let lex cases s =
      let rec l :(Reex.t * F.lexer_rhs) list -> last -> char list -> Term.t list =
        fun cases' k -> function
        | [] -> m k
        | c :: cs -> let cases'_c = without_unmatchables (deriv_rhss c cases') in
                     (* If all the derivatives are ⊥, we're done *)
                     if cases'_c = [] then m k
                     else (* At least one non-⊥ derivative.  We're going to consume c.
                             Update 'k' if we have an epsilon match, and continue lexing *)
                       match first_nullable cases'_c with
                          | None -> l cases'_c k cs
                          | Some k' -> l cases'_c (last_of_rhs cs k') cs

      and m = function
        | None                           -> failwith "lex"
        | Some (Skip,[])                 -> []
        | Some (Skip,(_ :: _ as rest))   -> l cases None rest
        | Some (Term t,[])               -> [t]
        | Some (Term t,(_ :: _ as rest)) -> t :: l cases None rest
      in
      match l cases None (explode s) with
      | exception Failure _ -> None
      | v -> Some v

  end

  module Normalized =
  struct
    module P = Map.Make(Term)

    let rec parse : type g p. g N.grammar -> p N.rhs -> Term.t list -> Term.t list option =
      fun g rhs -> function
      | [] -> map_null [] rhs.null
      | t :: ts -> match P.find_opt t rhs.prods with
                   | Some (Prod {nonterms;_}) -> parse_seq g ts nonterms
                   | None -> map_null (t :: ts) rhs.null

    and parse_seq : type g n. g N.grammar -> Term.t list -> n N.ntseq -> Term.t list option =
      fun g cs -> function
      | Empty -> Some cs
      | Cons (x, xs) -> match parse g N.Productions.(g.items.%{x}) cs with
                        | Some cs -> parse_seq g cs xs
                        | None -> None

    let recognize (g : _ N.grammar) (s : Term.t list) : bool =
      parse g N.Productions.(g.items.%{g.start}) s = Some []
  end

  module Fused =
  struct
    module F = Fused.Make(Term)
    type 'a rhss = (Reex.t * 'a F.cont) list

    type last = Match : char list * _ N.ntseq -> last | Fail

    let rec parse : type g p. g F.grammar -> p rhss -> char list * last -> char list option =
      fun g rhss -> function
      | [], Match (rest, nts) -> parse_seq g nts rest
      | [], Fail -> None
      | c :: cs, last -> let rhss'_c = without_unmatchables (deriv_rhss c rhss) in
                         (* Update 'last' if we have an epsilon match *)
                         let last' = match first_nullable rhss'_c with
                           | None                                    -> last
                           | Some (_, Success (N.Prod {nonterms;_})) -> Match (cs, nonterms)
                           | Some (_, Error _)                       -> Fail
                         in
                         if rhss'_c = [] then
                           match last with
                           (* If all the derivatives are ⊥, we're done *)
                           | Match (rest, nts) -> parse_seq g nts rest
                           | Fail              -> None
                         else (* At least one non-⊥ derivative.  Consume c and continue lexing. *)
                           parse g rhss'_c (cs, last')

    and parse_seq : type g n. g F.grammar -> n N.ntseq -> char list -> char list option =
      (* check whether the nonterminal sequence N1...Nn matches cs *)
      fun g nts cs ->
      match nts with
      | Empty -> Some cs
      | Cons (x, xs) -> let prods, eps = F.lookup g.items x in
                        match parse g prods (cs, if Option.is_some eps then Match (cs, Empty) else Fail) with
                        | Some cs -> parse_seq g xs cs
                        | None -> None

    let recognize (g : _ F.grammar) (s : string) : bool =
      let l = explode s in
      let prods, eps = F.lookup g.items g.start in
      parse g prods (l, if Option.is_some eps then Match (l, Empty) else Fail) = Some []
  end
end
