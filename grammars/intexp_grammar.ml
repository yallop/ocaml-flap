module T =
struct
  type t = LET | ID | IN | INT | TIMES | PLUS | MINUS
         | EQUAL | THEN | ELSE | IF | LPAREN | RPAREN [@@deriving ord, show]
end

open Grammars_common.Common(T)
open P
open Flap.Cd

type 'a term = (string * int) list -> 'a

let let_   = P.tok T.LET    @@ fun _ -> injv .<()>.
let id     = P.tok T.ID     @@ fun x -> x
let in_    = P.tok T.IN     @@ fun _ -> injv .<()>.
let int    = P.tok T.INT    @@ fun x -> injv .< int_of_string .~(dyn x) >.
let times  = P.tok T.TIMES  @@ fun _ -> injv .<()>.
let plus   = P.tok T.PLUS   @@ fun _ -> injv .<()>.
let minus  = P.tok T.MINUS  @@ fun _ -> injv .<()>.
let equal  = P.tok T.EQUAL  @@ fun _ -> injv .<()>.
let then_  = P.tok T.THEN   @@ fun _ -> injv .<()>.
let else_  = P.tok T.ELSE   @@ fun _ -> injv .<()>.
let if_    = P.tok T.IF     @@ fun _ -> injv .<()>.
let lparen = P.tok T.LPAREN @@ fun _ -> injv .<()>.
let rparen = P.tok T.RPAREN @@ fun _ -> injv .<()>.

let lexer = 
  L.[str "let"            , P.Return T.LET;
     str "in"             , P.Return T.IN;
     chr '*'              , P.Return T.TIMES;
     chr '+'              , P.Return T.PLUS;
     chr '-'              , P.Return T.MINUS;
     chr '='              , P.Return T.EQUAL;
     str "then"           , P.Return T.THEN;
     str "else"           , P.Return T.ELSE;
     str "if"             , P.Return T.IF;
     chr '('              , P.Return T.LPAREN;
     chr ')'              , P.Return T.RPAREN;
     alpha >>> star alnum , P.Return T.ID;
     plus digit           , P.Return T.INT;
     charset "\r\n \t"    , P.Skip;
  ]

let paren p =
  (lparen >>> p >>> rparen)
  $ (fun p -> inj .< Stdlib.snd (Stdlib.fst .~(dyn p)) >.)

let ident : int term P.t = id  $ fun x -> injv .< List.assoc .~(dyn x) >.
let int   : int term P.t = int $ fun x -> injv .< fun _ -> .~(dyn x) >.

let mkOp o e1 e2 = .< fun env -> .~o (.~e1 env) (.~e2 env) >.

let any gs = List.fold_left (<|>) bot gs

let maybe p = any [p $ (fun x -> injv .<Option.Some .~(dyn x)>.); eps (injv .<Option.None>.)]

let infixr f base op =
  fix (fun g -> base >>> maybe (op >>> g)
                $ (fun x -> inj .< match .~(dyn x) with
                   | (e, None) -> e
                   | (e, Some(o, e')) -> .~(f .<o>. .<e>. .<e'>.)>.))

let infixl f base op =
  base >>> star (op >>> base)
  $ fun p -> inj .< let (e, oes) = .~(dyn p) in
                    List.fold_left (fun e (o, e') -> .~(f .<o>. .<e>. .<e'>.)) e oes >.

type assoc = Left | Right

let mkInfix = function
  | Left -> infixl
  | Right -> infixr

let infix f base aos =
  List.fold_left (fun base (a, o) -> mkInfix a f base o) base aos


let exp = fix (fun exp ->
              let atom = any [paren exp; ident; int] in
              let eqexp = infix mkOp atom
                            [Left, (times $ fun _ -> injv .< ( * ) >.);
                             Left, any [(plus  $ fun _ -> injv .< ( + ) >.);
                                        (minus $ fun _ -> injv .< ( - ) >.);];
                             Left, equal $ fun _ -> injv .< fun x y -> if (x = y) then 1 else 0 >. ]
              in
              let letexp =
                (let_ >>> id >>> equal >>> exp >>> in_ >>> exp) $
                (fun p -> inj .< let (((((_,x),_),e),_),e') = .~(dyn p) in
                             fun env -> e' ((x,e env) :: env) >.) in
              let ifexp =
                (if_ >>> exp >>> then_ >>> exp >>> else_ >>> exp) $
                (fun p -> inj .< let (((((_,e1),_),e2),_),e3) = .~(dyn p) in
                             fun env -> if e1 env <> 0 then e2 env else e3 env>.)
              in
              any [eqexp; letexp; ifexp])

let code = Result.get_ok (build "intexp" exp lexer)
