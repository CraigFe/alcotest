open Higher

type speed_level = [ `Quick | `Slow ]

module Test_name : sig
  type t

  val v : name:string -> index:int -> t

  val name : t -> string

  val index : t -> int

  val pp : t Fmt.t
  (** Pretty-print the unescaped test-case name *)

  val file : t -> string
  (** An escaped form of the test name with [.output] suffix. *)

  val length : t -> int
  (** The approximate number of terminal columns consumed by [pp_name]. *)

  val compare : t -> t -> int
  (** Order lexicographically by name, then by index. *)
end

module Run_result : sig
  type t =
    [ `Ok
    | `Exn of Test_name.t * string * unit Fmt.t
    | `Error of Test_name.t * unit Fmt.t
    | `Skip
    | `Todo of string ]

  val is_failure : t -> bool
  (** [is_failure] holds for test results that are error states. *)
end

module Test : sig
  (** The set of operations available to the user for constructing test suites. *)
  module Dsl : sig
    type ('a, 'm) t

    val v : name:string -> ('a -> (unit, 'm) app) -> ('a, 'm) t

    val group : name:string -> ('a, 'm) t list -> ('a, 'm) t
  end

  type ('a, 'm) t

  val compile :
    ('a, 'm) Dsl.t ->
    (('a, 'm) t, [> `Duplicate_test_path of string | `Empty ]) result
  (** Copmile the user DSL to an intermediate form to use for executing the
      tests, rejecting any invalid states. *)

  val fold :
    (module Monad.S with type br = 'm) ->
    ('a, 'm) t ->
    'acc ->
    ('acc -> path:string list -> run:('a -> (unit, 'm) app) -> 'acc) ->
    ('acc, 'm) app
  (** Depth-first, left-to-right monadic traversal. *)
end

(* module Suite : sig
 *   type ('a, 'm) t
 * 
 *   val v : name:string -> (_ t, [> `Empty_name ]) result
 *   (\** Construct a new suite, given a non-empty [name]. Test cases must be added
 *       with {!add}. *\)
 * 
 *   val name : _ t -> string
 *   (\** An escaped form of the suite name. *\)
 * 
 *   val pp_name : _ t Fmt.t
 *   (\** Pretty-print the unescaped suite name. *\)
 * 
 *   val add :
 *     'a t -> 'a Test.t -> ('a t, [ `Duplicate_test_path of string ]) result
 * 
 *   val tests : 'a t -> 'a test_case list
 * 
 *   val doc_of_test_name : 'a t -> Test_name.t -> string
 * end *)
