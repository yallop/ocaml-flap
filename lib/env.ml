module Var =
struct
  type 'a id = ..
  type 'a t = { id: 'a id;
                str : string;
                eql : 'b. 'b id -> ('a, 'b) Letrec.eql option }
  let counter = ref 0
  let var : type a. unit -> a t =
    fun () ->
    let module M = struct
          type _ id += Id : a id
          let str = incr counter; Printf.sprintf "x%d" !counter
          let eql : type b. b id -> (a, b) Letrec.eql option =
            function Id -> Some Refl | _ -> None
      end
    in M.{ id = Id; eql; str }
  let eql {eql;_} {id;_} = eql id
  let str {str;_} = str
end

module Env (V: sig type 'a t end) =
struct
  type entry = E : 'a Var.t * 'a V.t -> entry | Alias : 'a Var.t * 'a Var.t -> entry
(* TODO: can we make this a bit more well-formed? *) 
  type t = entry list
  let empty = []
  let remove k env =
    let f = function
        E (x,_) -> (match Var.eql x k with Some _ -> false | _ -> true)
      | Alias (x, _)  -> (match Var.eql x k with Some _ -> false | _ -> true)in
    List.filter f env
  let bind k v env = E (k, v) :: remove k env
  let alias x y env = Alias (x, y) :: remove x env
  let extend l r =
    let f e r =
      match e with
      | E (k, v) -> bind k v r
      | Alias (x, y) -> alias x y r
    in
    List.fold_right f l r
  let (.%{}) = (* Warning: this can loop if there are cyclic aliases *)
    let rec go : type a. a Var.t -> t -> t -> a V.t =
    fun x orig -> function
    | [] -> raise Not_found
    | Alias (y, z) :: env -> (match Var.eql x y with Some Refl -> go z orig orig | None -> go x orig env)
    | E (y, v) :: env -> (match Var.eql x y with Some Refl -> v | None -> go x orig env)
    in fun l x -> go x l l

  let canonicalize =
    let rec go : type a. a Var.t -> t -> t -> a Var.t =
    fun x orig -> function
    | [] -> x
    | E (y, _) :: env -> (match Var.eql x y with Some Refl -> x | None -> go x orig env)
    | Alias (y, z) :: env -> (match Var.eql x y with Some Refl -> go z orig orig | None -> go x orig env)
    in fun x l -> go x l l

  let pp (f : <m:'a.Format.formatter -> 'a V.t -> unit>) fmt p =
    let g = function
      | E (x, v) -> Format.fprintf fmt "@[%s ↦ @[%a@]@],@ " (Var.str x) f#m v
      | Alias (x, y) -> Format.fprintf fmt "@[%s ↝ %s@],@ " (Var.str x) (Var.str y) in
    (Format.fprintf fmt "{"; List.iter g p; Format.fprintf fmt "}")
end
