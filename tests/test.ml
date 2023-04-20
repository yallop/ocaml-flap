open OUnit2

module Grammars =
struct
  module S = struct include Sexp_grammar end
  module P = struct include Pgn_grammar end
  module Q = struct include Ppm_grammar end
  module J = struct include Json_grammar end
  module C = struct include Csv_grammar end
  module I = struct include Intexp_grammar end
end

module Single_rec =
struct
  module C = Flap.Cd
  module T = struct type t = LPAREN | RPAREN | SYMBOL [@@deriving ord,show] end
  module P = Flap.Parse(T)
  module R = Flap__.Recognizer.Make(T)

  let cases =
     P.(Reex.[chr '(',                                Return LPAREN;
              range 'a' 'z' >>> star (range 'a' 'z'), Return SYMBOL;
              chr ')',                                Return RPAREN;
              chr ' ',                                Skip])

  let (inj, injv, dyn) = C.(inj, injv, dyn)
  open P

  let tok c = P.tok c @@ fun c -> c

  let star x =
    fix @@ fun starx ->
    (eps (injv .<[]>.))
     <|> 
    (x >>> starx $ fun p -> C.let_ p @@ fun p -> injv .< .~C.(dyn (fst p)) :: .~C.(dyn (snd p)) >.)
  
  let sexp : ([`Atom | `Sexp of 'sexp list] as 'sexp) t =
    fix @@ fun sexp ->
      (tok T.SYMBOL $ fun _ -> injv .< `Atom >.)
  <|> (tok T.LPAREN >>> star sexp >>> tok T.RPAREN $ fun p -> C.let_ p @@ fun p -> injv .< `Sexp .~C.(dyn (snd (fst p))) >.)
  
  let compiled' = match P.compile cases sexp with Ok c -> c | Error e -> failwith e
  (* let () = Format.(fprintf err_formatter) "%a@." Codelib.print_code compiled' *)
  
  let parser = Runnative.run compiled'
end


module Backtracking =
struct
  module C = Flap.Cd
  module T = struct type t = AA | AAAB | EOF [@@deriving ord,show] end
  module P = Flap.Parse(T)

  let cases =
    P.(Reex.[str "aa",   Return AA;
             str "aaab", Return AAAB;
             str "",     Return EOF])

  let (inj, injv, dyn) = C.(inj, injv, dyn)
  open P

  let tok c = P.tok c @@ fun c -> c

  let star x =
    fix @@ fun starx ->
    (eps (injv .<[]>.))
     <|> 
    (x >>> starx $ fun p -> C.let_ p @@ fun p -> injv .< .~C.(dyn (fst p)) :: .~C.(dyn (snd p)) >.)
  
  let aa () = tok T.AA $ fun _ -> injv .< `AA >.
  let aaab () = tok T.AAAB $ fun _ -> injv .< `AAAB >.

 let grammar : [ `AA | `AAAB ] list t = star (aa ()  <|> aaab ())
  
  let compiled' = match P.compile cases grammar with Ok c -> c | Error e -> failwith e
  
  let parser = Runnative.run compiled'
end


module Sexp_recognizer =
struct
  module C = Flap__.Code
  module T = struct type t = LPAREN | RPAREN | SYMBOL [@@deriving ord,show] end
  module S = Set.Make(T)
  module R = Flap__.Recognizer.Make(T)
  module F = Flap__.Fused.Make(T)
  module N = Flap__.Normalize.Make(T)
  open N

  let cases =
    Reex.[chr '(',                                F.Return LPAREN;
          range 'a' 'z' >>> star (range 'a' 'z'), F.Return SYMBOL;
          chr ')',                                F.Return RPAREN;
          chr ' ',                                F.Skip]

  let (inj, injv, dyn) = C.(inj, injv, dyn)

  let tok c = N.tok (c $$ fun c -> c)

 let startset = S.of_list T.[LPAREN; SYMBOL]

  let star x =
    fix startset @@ fun starx ->
    (eps (injv .<[]>.))
     <|> 
    (x >>> starx $ fun p -> C.let_ p @@ fun p -> injv .< .~C.(dyn (fst p)) :: .~C.(dyn (snd p)) >.)
  
  let sexp : ([`Atom | `Sexp of 'sexp list] as 'sexp) t =
    fix startset @@ fun sexp ->
      (tok T.SYMBOL $ fun _ -> injv .< `Atom >.)
  <|> (tok T.LPAREN >>> star sexp >>> tok T.RPAREN $ fun p -> C.let_ p @@ fun p -> injv .< `Sexp .~C.(dyn (snd (fst p))) >.)
  
  let fused = F.fuse cases (N.resolve sexp)
  (* let () = Format.(fprintf err_formatter) "%a@." (F.pp_grammar (fun _ _ -> ())) fused *)

  let unfused s =
    match R.Lexer.lex cases s with
    | None -> false
    | Some ts -> R.Normalized.recognize (N.resolve sexp) ts
end


module Tok_recognizer =
struct
  module C = Flap__.Code
  module T = struct type t = AA | AAAB [@@deriving ord,show] end
  module S = Set.Make(T)
  module R = Flap__.Recognizer.Make(T)
  module F = Flap__.Fused.Make(T)
  module N = Flap__.Normalize.Make(T)
  open N

  let cases =
    Reex.[str "aa", F.Return AA;
          str "aaab", F.Return AAAB]

  let (inj, injv, dyn) = C.(inj, injv, dyn)

  let tok c = N.tok (c $$ fun c -> c)

  let star x =
    fix  (S.of_list T.[AA; AAAB]) @@ fun starx ->
    (eps (injv .<[]>.))
     <|> 
    (x >>> starx $ fun p -> C.let_ p @@ fun p -> injv .< .~C.(dyn (fst p)) :: .~C.(dyn (snd p)) >.)

  let grammar : _ t = star (tok T.AA <|> tok T.AAAB) (* >>> tok T.EOF *)
  
  let fused = F.fuse cases (N.resolve grammar)
  (* let () = Format.(fprintf err_formatter) "%a@." (F.pp_grammar (fun _ _ -> ())) fused *)

  let unfused s =
    match R.Lexer.lex cases s with
    | None -> false
    | Some ts -> R.Normalized.recognize (N.resolve grammar) ts
end


let single_rec_sexp_tests _ =
  begin
    assert_equal
      (`Atom)
      (Single_rec.parser "abc-def");
    
    assert_equal
      (`Sexp [])
      (Single_rec.parser "()");

    assert_equal
      (`Sexp [`Atom;])
      (Single_rec.parser "( aaa )");
    
    assert_equal
      (`Sexp [`Atom; `Sexp [`Atom; `Sexp []]; `Atom;])
      (Single_rec.parser "  (a(aa())  a)");
  end


let backtracking _ =
  begin
    assert_equal [`AA] (Backtracking.parser "aa");

    assert_equal [`AA;`AA] (Backtracking.parser "aaaa");

    assert_equal [`AAAB] (Backtracking.parser "aaab");
  end


let recognizer_sexp_tests _ =
  begin
    assert
      (Sexp_recognizer.(R.Fused.recognize fused) "()");

    assert
      (Sexp_recognizer.unfused "()");

    assert
      (Sexp_recognizer.(R.Fused.recognize fused) "abcdef");

    assert
      (Sexp_recognizer.unfused "abcdef");

    assert
      (Sexp_recognizer.(R.Fused.recognize fused) "( abc )");

    assert
      (Sexp_recognizer.unfused "( abc )");
    
    assert
      (not (Sexp_recognizer.(R.Fused.recognize fused) "( abc ))"));

    assert
      (not (Sexp_recognizer.unfused "( abc ))"));
    
    assert
      (not (Sexp_recognizer.(R.Fused.recognize fused) "("));

    assert
      (not (Sexp_recognizer.unfused "("));

    assert
      (Sexp_recognizer.(R.Fused.recognize fused) "( (abc) ((h bc) (ef g)) )");

    assert
      (Sexp_recognizer.unfused "( (abc) ((h bc) (ef g)) )");

    assert
      (not (Sexp_recognizer.(R.Fused.recognize fused) "( (abc) ((h bc (ef g)) )"));

    assert
      (not (Sexp_recognizer.unfused "( (abc) ((h bc (ef g)) )"));

    assert
      (not (Sexp_recognizer.(R.Fused.recognize fused) "( (abc) ((h bc) (ef g)) )."));

    assert
      (not (Sexp_recognizer.unfused "( (abc) ((h bc) (ef g)) )."));
  end


let tok_recognizer_tests _ =
  begin
    assert
      (Tok_recognizer.(R.Fused.recognize fused) "aa");

    assert
      (Tok_recognizer.unfused "aa");

    assert
      (not (Tok_recognizer.(R.Fused.recognize fused) "aaa"));

    assert
      (not (Tok_recognizer.unfused "aaa"));
    
    assert
      (Tok_recognizer.(R.Fused.recognize fused) "aaaa");

    assert
      (Tok_recognizer.unfused "aaaa");
    
    assert
      (Tok_recognizer.(R.Fused.recognize fused) "aaab");

    assert
      (Tok_recognizer.unfused "aaab");
  end

let suite = "Parser tests" >:::
  ["single rec sexp"
    >:: single_rec_sexp_tests;
   
   "backtracking parser"
    >:: backtracking;
   
   "recognzier sexp"
    >:: recognizer_sexp_tests;

   "tok recognzier"
    >:: tok_recognizer_tests;
  ]


let _ =
  run_test_tt_main suite
