open! Import

(** A universal type, into which any other type can be injected and (partially)
    recovered. *)
module Univ : sig
  type t

  val create : unit -> ('a -> t) * (t -> 'a option)
end = struct
  type t = ..

  let create : type a. unit -> (a -> t) * (t -> a option) =
   fun () ->
    let module M = struct
      type t += Case of a option
    end in
    ((fun x -> M.Case (Some x)), function M.Case x -> x | _ -> None)
end

type 'a spec = {
  type_id : 'a Type_id.t;
  id : int;
  inj : 'a -> Univ.t;
  prj : Univ.t -> 'a option;
  name : string;
  pp : Format.formatter -> 'a -> unit;
}

let gen_id =
  let counter = ref (-1) in
  fun () ->
    incr counter;
    !counter

let spec ~name ~pp =
  let inj, prj = Univ.create () in
  { type_id = Type_id.create (); id = gen_id (); inj; prj; name; pp }

type t = V : 'a spec * 'a -> t

let pp ppf (V ({ name; pp; _ }, v)) = Fmt.pf ppf "%s = %a" name pp v
let pp_spec ppf spec = Format.fprintf ppf "(spec %s)" spec.name
let const ~name v = V (spec ~name ~pp:Fmt.(const string name), v)

module Set = struct
  (** A tag set is a map from (boxed) definitions to tags. *)

  module Key = struct
    type t = V : 'a spec -> t [@@ocaml.unboxed]

    let compare (V x) (V y) = Int.compare x.id y.id
  end

  module Map = Map.Make (Key)

  type nonrec t = t Map.t

  let empty = Map.empty
  let add (V (k, _) as v) s = Map.add (Key.V k) v s

  let find : type a. a spec -> t -> a option =
   fun k t ->
    try
      let (V (k', v)) = Map.find (Key.V k) t in
      match Type_id.equal k.type_id k'.type_id with
      | Some Refl -> Some v
      | None -> assert false
    with Not_found -> None

  let to_list = Map.to_seq >> List.of_seq >> List.map snd

  let fold_until =
    let rec aux ~f ~finish acc seq =
      match seq () with
      | Seq.Cons ((_k, v), xf) -> (
          match f acc v with
          | Continue acc -> aux ~f ~finish acc xf
          | Stop final -> final)
      | Seq.Nil -> finish acc
    in
    fun t ~init ~f ~finish -> aux ~f ~finish init (Map.to_seq t)
end

module Speed_level = struct
  type t = [ `Quick | `Slow ]

  let tag =
    spec ~name:"speed_level"
      ~pp:(Fmt.of_to_string (function `Quick -> "Quick" | `Slow -> "Slow"))

  let quick = V (tag, `Quick)
  let slow = V (tag, `Slow)

  let without_slow s =
    match Set.find tag s with Some `Slow -> `Skip | _ -> `Run
end

module Predicate = struct
  type t = unit -> [ `Run | `Skip ]

  let tag =
    spec ~name:"Predicate" ~pp:(fun ppf _ -> Fmt.pf ppf "Predicate <...>")

  let only_if s = match Set.find tag s with Some p -> p () | None -> `Run
end

module Position = struct
  let tag = spec ~name:"index" ~pp:Fmt.(using snd int)
end

module Filter = struct
  type t = Set.t -> [ `Run | `Skip ]

  let ( ++ ) f g x = match (f x, g x) with `Run, `Run -> `Run | _, _ -> `Skip
  let default = Predicate.only_if (* TODO: be sensitive to `quick_only` *)
end