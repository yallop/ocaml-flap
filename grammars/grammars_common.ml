module Common(Term: sig type t [@@deriving ord, show] end) =
struct
  module P =
  struct
    open Flap.Cd
    include Flap.Parse(Term)
    (* include Flap_compiler.Cd
     * include Flap.Parse(T)(Flap_compiler.Cd)
     * include Flap_gnf.Make(T)(Flap_compiler.Cd) *)
    let option e = (e $ fun x -> let_ x @@ fun x -> injv .< Option.Some .~(dyn x) >.) <|> (eps (injv .<Option.None>.))
    let star e = fix @@ fun x -> (eps (injv .<[]>.)
                                  <|> (e >>> x $ fun p -> let_ p @@ fun p ->
                                                          injv .< .~(dyn (fst p)) :: .~(dyn (snd p)) >.))
    let oneormore e = (e >>> star e) $ fun p -> let_ p @@ fun p -> injv .< .~(dyn (fst p)) :: .~(dyn (snd p)) >.
    (* include Flap_compiler.Make2(T) *)
  end

  module L =
  struct
    include Reex
    (* Various useful regex combinators *)
    let charset s = Seq.fold_left (fun s c -> s <|> chr c) empty (String.to_seq s)
    let upper = range 'A' 'Z'
    let lower = range 'a' 'z'
    let digit = range '0' '9'
    let alpha = upper <|> lower
    let alnum = alpha <|> digit
    let decimal = plus digit >>> opt (chr '.' >>> plus digit)
    let complement s = any <&> not (charset s)
    let stringchar = complement "\"\\" <|> (chr '\\' >>> any)
    let string = chr '"' >>> star stringchar >>> chr '"'

    let (==>) ss v = (ss, P.Return v)
    let skip ss = (ss, P.Skip)
    let reject ss msg = (ss, P.Error msg)
    let (!) = regex
  end

  let build name parser lexer =
    let start_time = Sys.time () in
    let fused = P.compile lexer parser in
    let after_fusion = Sys.time () in
    Printf.fprintf stderr "[%s]: compilation time : %gms\n" name ((after_fusion -. start_time) *. 1000.);
    fused
end
