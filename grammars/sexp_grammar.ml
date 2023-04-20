module T = struct type t = LPAREN | RPAREN | ATOM [@@deriving ord, show] end

open Grammars_common.Common(T)
open Flap.Cd

let token t v = P.tok t @@ fun _ -> injv v
let lparen = token T.LPAREN .<()>.
let rparen = token T.RPAREN .<()>.
let atom = P.tok T.ATOM @@ fun s -> s

let lexer =
  L.[ alpha >>> star alnum , P.Return ATOM;
      charset "\r\n \t"    , P.Skip;
      chr '('              , P.Return LPAREN;
      chr ')'              , P.Return RPAREN;
  ]

let parser = let open P in
  fix @@ fun sexp ->
   ((lparen >>> star sexp >>> rparen)
     $ fun p -> let_ p @@ fun p ->
                injv .<List.fold_left (+) 0 (Stdlib.snd (Stdlib.fst .~(dyn p)))  >.)
   <|>
   (atom $ fun _ -> injv .<1>.)

let code = Result.get_ok (build "sexp" parser lexer)
