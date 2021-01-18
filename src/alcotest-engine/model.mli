open! Import

module Run_result : sig
  type index = Safe_string.t list * Safe_string.t

  type t =
    | Ok
    | Exn of index * string * unit Fmt.t
    | Error of index * unit Fmt.t
    | Skip
    | Todo of string

  val is_failure : t -> bool
  (** [is_failure] holds for test results that are error states. *)

  val has_run : t -> bool
end

module Suite (M : Monad.S) : sig
  type 'a test

  val test :
    name:string ->
    loc:Lexing.position option ->
    tags:Tag.Set.t ->
    ('a -> unit M.t) ->
    'a test

  val group :
    name:string ->
    loc:Lexing.position option ->
    tags:Tag.Set.t ->
    'a test list ->
    'a test

  type 'a t

  type 'a with_context :=
    < path : Safe_string.t list ; name : Safe_string.t ; last : bool > -> 'a

  val foldi_until :
    filter:Tag.Filter.t ->
    ?group:
      ('acc -> [ `Run | `Skip ] -> ('acc, 'final) continue_or_stop M.t)
      with_context ->
    ?test:
      ('acc ->
      [ `Run of 'a -> unit M.t | `Skip ] ->
      ('acc, 'final) continue_or_stop M.t)
      with_context ->
    init:'acc ->
    finish:('acc -> 'final) ->
    'a t ->
    'final M.t
  (** Depth-first traversal over the suite, skipping nodes according to the
      [filter] predicate defined over node {!Tag}s. *)

  val fold :
    filter:Tag.Filter.t ->
    group:('acc -> [ `Run | `Skip ] -> 'acc) ->
    test:('acc -> [ `Run of 'a -> unit M.t | `Skip ] -> 'acc) ->
    init:'acc ->
    'a t ->
    'acc M.t

  val of_tests :
    name:string ->
    loc:Lexing.position option ->
    'a test list ->
    ('a t, [ `Duplicate_path of string | `Empty_name ]) result

  val name : _ t -> string
  (** An escaped form of the suite name. *)

  val pp_name : _ t Fmt.t
  (** Pretty-print the unescaped suite name. *)

  val pp : _ t Fmt.t
end
