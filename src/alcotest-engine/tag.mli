open! Import

type 'a spec

val spec : name:string -> pp:(Format.formatter -> 'a -> unit) -> 'a spec

type t = V : 'a spec * 'a -> t

val pp : t Fmt.t
val pp_spec : _ spec Fmt.t
val const : name:string -> 'a -> t

module Set : sig
  type tag
  type t

  val empty : t
  val add : tag -> t -> t
  val find : 'a spec -> t -> 'a option
  val to_list : t -> tag list

  val fold_until :
    t ->
    init:'a ->
    f:('a -> tag -> ('a, 'b) continue_or_stop) ->
    finish:('a -> 'b) ->
    'b
end
with type tag := t

module Filter : sig
  type t = Set.t -> [ `Run | `Skip ]

  val ( ++ ) : t -> t -> t
  (** [f ++ g] is the filter that runs only tests that are run by both [f]
      {i and} [g]. *)

  val default : t
end

module Speed_level : sig
  type tag
  type t = [ `Quick | `Slow ]

  val tag : t spec
  val quick : tag
  val slow : tag
  val without_slow : Filter.t
end
with type tag := t

module Position : sig
  val tag : (string * int) spec
end

module Predicate : sig
  type t = unit -> [ `Run | `Skip ]

  val tag : t spec
  val only_if : Filter.t
end
