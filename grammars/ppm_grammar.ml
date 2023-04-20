module T = struct type t = INT | P3  [@@deriving ord, show] end

open Grammars_common.Common(T)
open Flap.Cd

let int = P.tok T.INT @@ fun s -> injv .<int_of_string .~(dyn s)>.
let p3 = P.tok T.P3 @@ fun _ -> injv .<()>.

let lexer =
  L.[ str "P3"                          , P.Return P3;
      plus digit                        , P.Return INT;
      chr '#' >>> star (not (chr '\n')) , P.Skip;
      chr '\n'                          , P.Skip; (* TODO: record newline *)
      charset "\r\n \t"                 , P.Skip;
      any                               , P.Error "Unexpected character"
  ]

let ints = P.(fix @@ fun ints ->
                     (eps (injv .< (min_int, 0) >.))
                     <|>
                     (int >>> ints $ fun p -> inj .< let (i, (m,count)) = .~(dyn p) in
                                                 (max i m, succ count) >.))

let exp = P.((p3 >>> int >>> int >>> int >>> ints) $
               fun p -> inj .< let ((((_,w),h),max),(max',count)) = .~(dyn p) in
                           (max' <= max) && (count = 3 * w * h) >.)

let code = Result.get_ok (build "ppm" exp lexer)
