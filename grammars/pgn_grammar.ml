module T =
struct
  type t = COORDINATE | CASTLE | STAR | INT | INTDOT | MINUS | SLASH
         | STRING | LBRACKET | RBRACKET | TAG [@@deriving ord, show]
end

open Grammars_common.Common(T)

let lexer =
  L.[chr '['                                    ==> LBRACKET;
    ! "]"                                       ==> RBRACKET;
    ! "/"                                       ==> SLASH;
    ! "-"                                       ==> MINUS;
    ! "[*]"                                     ==> STAR;
      string                                    ==> STRING;
      plus alpha >>> digit >>> 
      star (charset "=#-+" <|> alnum)           ==> COORDINATE;
    ! "O-O(-O)?([#+])?"                         ==> CASTLE;
      plus digit >>> chr '.'                    ==> INTDOT;
      plus digit                                ==> INT;
      upper >>> star alpha                      ==> TAG;
      skip !"[\n]";                                 (* TODO: count newline *)
      skip !"[\r\n \t]";
      (* reject any "Unexpected character" *)]

module Parser =
struct
  open P
  open Flap.Cd

  let (/=>) t f = P.tok t @@ fun s -> injv (f s)
  let coordinate = T.COORDINATE /=> fun s -> dyn s
  let castle     = T.CASTLE     /=> fun _ -> .<()>.
  let star_      = T.STAR       /=> fun _ -> .<()>.
  let int        = T.INT        /=> fun s -> dyn s
  let intdot     = T.INTDOT     /=> fun _ -> .<()>.
  let minus      = T.MINUS      /=> fun _ -> .<()>.
  let slash      = T.SLASH      /=> fun _ -> .<()>.
  let string     = T.STRING     /=> fun s -> dyn s
  let lbracket   = T.LBRACKET   /=> fun _ -> .<()>.
  let rbracket   = T.RBRACKET   /=> fun _ -> .<()>.
  let tag        = T.TAG        /=> fun s -> dyn s

  let coordinate : [`CASTLE | `COORD of string ] P.t
                 = (coordinate $ fun s -> injv .<`COORD .~(dyn s)>.)
               <|> (castle     $ fun _ -> injv .<`CASTLE>.)

  let move = intdot >>> coordinate >>> option coordinate

  let result : [`DRAWN | `OTHER | `WON ] P.t =
    (star_ $ fun p -> let_ p @@ fun _ -> injv .<`OTHER>.)
    <|>
      ((int >>>
         ((minus >>> int $ fun p -> let_ p @@ fun _ -> injv .<`WON>.)
          <|>
          (slash >>> int >>> minus >>> int >>> slash >>> int $ fun p -> let_ p @@ fun _ -> injv .<`DRAWN>.)))
       $ fun p -> snd p )

  let moves = star move

  let metadatum =
    lbracket >>> tag >>> string >>> rbracket
    $ fun p -> snd (fst p)

  let metadata = oneormore metadatum

  let game = metadata >>> moves >>> result $ fun p -> snd p

  let games =  oneormore game
end

let code = Result.get_ok (build "pgn" Parser.games lexer)
