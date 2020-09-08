module type S = sig
  type 'a t

  include Higher.BRANDED with type 'a t := 'a t

  val return : 'a -> 'a t

  val bind : 'a t -> ('a -> 'b t) -> 'b t

  val catch : (unit -> 'a t) -> (exn -> 'a t) -> 'a t
end

module type EXTENDED = sig
  include S

  module Infix : sig
    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

    val ( >|= ) : 'a t -> ('a -> 'b) -> 'b t
  end

  module List : sig
    val fold_map_s :
      ('acc -> 'a -> ('acc * 'b) t) -> 'acc -> 'a list -> 'b list t
  end
end

module type Monad = sig
  module type S = S

  module type EXTENDED = EXTENDED

  module Identity : S with type 'a t = 'a

  module Extend (M : S) : EXTENDED with type 'a t = 'a M.t
end
