module T =
struct
  type t = COMMA | COLON | STRING | LBRACE | RBRACE
         | LBRACKET | RBRACKET | DECIMAL | NULL | TRUE | FALSE [@@deriving ord, show]
end

open Grammars_common.Common(T)
open Flap.Cd

let (/=>) t f = P.tok t @@ fun s -> injv (f s)
let comma    = T.COMMA    /=> fun _ -> .<()>.
let colon    = T.COLON    /=> fun _ -> .<()>.
let string_  = T.STRING   /=> fun s -> dyn s
let lbrace   = T.LBRACE   /=> fun _ -> .<()>.
let rbrace   = T.RBRACE   /=> fun _ -> .<()>.
let lbracket = T.LBRACKET /=> fun _ -> .<()>.
let rbracket = T.RBRACKET /=> fun _ -> .<()>.
let decimal_ = T.DECIMAL  /=> fun s -> dyn s
let null     = T.NULL     /=> fun _ -> .<()>.
let true_    = T.TRUE     /=> fun _ -> .<()>.
let false_   = T.FALSE    /=> fun _ -> .<()>.

let lexer =
  L.[
       chr '['           , P.Return LBRACKET;
       chr ']'           , P.Return RBRACKET;
       chr '{'           , P.Return LBRACE;
       chr '}'           , P.Return RBRACE;
       chr ','           , P.Return COMMA;
       chr ':'           , P.Return COLON;
       str "null"        , P.Return NULL;
       str "true"        , P.Return TRUE;
       str "false"       , P.Return FALSE;
       string            , P.Return STRING;
       decimal           , P.Return DECIMAL;
       charset "\r\n \t" , P.Skip;
  ]

let commasep p =
  P.(fix @@ fun ps ->
     eps (injv .<0>.) <|> (p >>> option (comma >>> ps) $ fun p -> let_ p @@ fun p ->
     inj .< match .~(dyn p) with
        | (x,None) -> x
        | (x,Some (_,xs)) -> x + xs >.))
let delim l p r =
  P.((l >>> p >>> r) $ fun p -> injv .< let ((_,v),_) = .~(dyn p) in  v >.)

let value = P.(fix @@ fun value ->
  let member = string_ >>>
                 option (colon >>> value) $
                 fun p -> let_ p @@ fun p -> inj .< match .~(dyn p) with  (_,None) -> 1
                                                 | (_,Some(_,v)) -> 1 + v >. in
  let obj = delim lbrace (commasep member) rbrace
  and arr = delim lbracket (commasep value) rbracket
  in
  obj <|> arr <|> (string_ $ fun _ -> injv .<1>.) <|>
  (decimal_ $ fun _ -> injv .<1>.) <|>
  (null $ fun _ -> injv .<1>.) <|>
  (true_ $ fun _ -> injv .<1>.) <|>
  (false_ $ fun _ -> injv .<1>.))

let values = P.(fix @@ fun values ->
  eps (injv .<0>.) <|> (value >>> values $ fun p -> let_ p @@ fun p -> inj .< Stdlib.fst .~(dyn p) + Stdlib.snd .~(dyn p) >.))

let parser = values

let code = Result.get_ok (build "json" parser lexer)
