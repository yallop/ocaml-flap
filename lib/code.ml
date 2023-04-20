type 'a value = Atomic of 'a code
              | Pair : 'a value * 'b value -> ('a * 'b) value

type 'a cd = Value of 'a value
           | LetComp : 'a code * ('a code -> 'b cd) -> 'b cd

type 'a t = 'a cd

let rec fst : type a b. (a * b) cd -> a cd = function
  | Value (Atomic v) -> Value (Atomic .<Stdlib.fst .~v>.)
  | Value (Pair (x,_)) -> Value x
  | LetComp (e, k) -> LetComp (e, fun x -> fst (k x))

let rec snd : type a b. (a * b) cd -> b cd = function
  | Value (Atomic v) -> Value (Atomic .<Stdlib.snd .~v>.)
  | Value (Pair (_,y)) -> Value y
  | LetComp (e, k) -> LetComp (e, fun x -> snd (k x))

let rec pair : type a b. a cd -> b cd -> (a * b) cd =
  fun x y ->
  match x, y with
  | Value x, Value y -> Value (Pair (x, y))
  | LetComp (e, k), e2 -> LetComp (e, fun x -> pair (k x) e2)
  | Value x, LetComp (e, k) -> LetComp (e, fun y -> pair (Value x) (k y))

let inj : type a. a code -> a cd = fun e -> LetComp (e, fun x -> Value (Atomic x))
let injv : type a. a code -> a cd = fun v -> Value (Atomic v)
let rec dyn : type a. a cd -> a code = function
  | Value (Atomic c) -> c
  | Value (Pair (x, y)) -> .< (.~(dyn (Value x)), .~(dyn (Value y))) >.
  | LetComp (e, k) -> .< let x = .~e in .~(dyn (k .<x>.)) >.

let rec let_ : type a b. a cd -> (a cd -> b cd) -> b cd =
  fun e k ->
  match e with
    Value _ as v -> k v
  | LetComp (e1, k1) -> LetComp (e1, fun x -> let_ (k1 x) k)

let unit = Value (Atomic .<()>.)
