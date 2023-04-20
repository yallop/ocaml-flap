(* CSV parsing, mostly according to RFC 4180 but with
   - no special treatment of headers
   - mandatory terminating CRLF
   The grammar is regular, so we don't strictly need to split it into lexer and parser.
   But it's useful to split it this way to illustrate the point.

   file = [header CRLF] record *(CRLF record) [CRLF]
   header = name *(COMMA name)
   record = field *(COMMA field)
   name = field
   field = (escaped / non-escaped)
   escaped = DQUOTE *(TEXTDATA / COMMA / CR / LF / 2DQUOTE) DQUOTE
   non-escaped = *TEXTDATA
   COMMA = %x2C
   CR = %x0D ;as per section 6.1 of RFC 2234 [2]
   DQUOTE =  %x22 ;as per section 6.1 of RFC 2234 [2]
   LF = %x0A ;as per section 6.1 of RFC 2234 [2]
   CRLF = CR LF ;as per section 6.1 of RFC 2234 [2]
   TEXTDATA =  %x20-21 / %x23-2B / %x2D-7E
 *)
module T = struct type t = CRLF | COMMA | FIELD [@@deriving ord, show] end

open Grammars_common.Common(T)
open Flap.Cd

let lexer =
  let open L in
  let textdata = range '\x20' '\x21'
             <|> range '\x23' '\x2B'
             <|> range '\x2D' '\x7E' in
  let dquote = chr '"' in
  let doubledquote = dquote >>> dquote in
  let comma = chr ',' in
  let cr = chr '\r' in
  let lf = chr '\n' in
  let crlf = cr >>> lf in
  let nonescaped = plus textdata in (* TODO: not quite right: we should have star textdata here *)
  let escaped    = dquote
               >>> star (textdata  <|> comma <|> cr <|> lf <|> doubledquote)
               >>> dquote in
  [comma                  ==> COMMA;
   crlf                   ==> CRLF;
   escaped <|> nonescaped ==> FIELD]

module Parser =
struct
  open P

  let (/=>) t f = tok t @@ fun s -> injv (f s)
  let comma = T.COMMA /=> fun _ -> .<()>.
  let crlf  = T.CRLF  /=> fun _ -> .<()>.
  let field = T.FIELD /=> fun s -> dyn s
  
  let fields = fix @@ fun fields -> (eps (injv .<0>.))
                                <|> ((comma >>> field) >>> fields $ fun p -> let_ p @@ fun p -> injv .< 1 + .~(dyn (snd p)) >.)
  
  let record = (field >>> fields) >>> crlf $ fun p -> let_ p @@ fun p ->injv (.<1 + .~(dyn (snd (fst p))) >.)
  let records = fix @@ fun records -> (record >>> option records $ fun p -> let_ p @@ fun p -> 
                                                                            let r = dyn (fst p) and rs = dyn (snd p) in
                                                                     inj .< match .~rs with
                                                                            | None -> .~r
                                                                            | Some rs -> assert (.~r = rs); .~r >.)
  let file = records
end

(* let code0 = build "csv" Parser.field lexer
 * let () = Format.(fprintf std_formatter) "let field = %a@." Codelib.print_code code0
 * 
 * let code1 = build "csv" Parser.fields lexer
 * let () = Format.(fprintf std_formatter) "let fields = %a@." Codelib.print_code code1
 * 
 * let code2 = build "csv" Parser.record lexer
 * let () = Format.(fprintf std_formatter) "let record = %a@." Codelib.print_code code2
 * 
 * let code3 = build "csv" Parser.records lexer
 * let () = Format.(fprintf std_formatter) "let records = %a@." Codelib.print_code code3 *)

let code = Result.get_ok (build "csv" Parser.file lexer)
(* let () = Format.(fprintf std_formatter) "let csv = %a@." Codelib.print_code code *)
