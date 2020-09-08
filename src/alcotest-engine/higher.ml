type ('a, 'fn) app

module type BRANDED = sig
  type 'a t

  type br

  val inj : 'a t -> ('a, br) app

  val prj : ('a, br) app -> 'a t
end

module Make (T : sig
  type 'a t
end) : BRANDED with type 'a t := 'a T.t = struct
  type br

  external inj : 'a T.t -> ('a, br) app = "%identity"

  external prj : ('a, br) app -> 'a T.t = "%identity"
end
