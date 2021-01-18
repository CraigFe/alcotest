(*
 * Copyright (c) 2017 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

(** [Alcotest_lwt] enables testing functions which return an Lwt promise. {!run}
    returns a promise that runs the tests when scheduled, catching any
    asynchronous exceptions thrown by the tests. *)

module V1 : sig
  include Alcotest_engine.V1.Cli.S with type return = unit Lwt.t

  val test_case :
    string -> speed_level -> (Lwt_switch.t -> 'a -> unit Lwt.t) -> 'a test_case

  val test_case_sync : string -> speed_level -> ('a -> unit) -> 'a test_case
end

module Unstable : sig
  open Alcotest_engine.Unstable

  include
    Cli.S
      with type 'a m := 'a Lwt.t
       and type 'a test_args := Lwt_switch.t -> 'a
       and type config := Config.User.t
       and type tag := Tag.t
       and type tag_set := Tag.Set.t

  val test_sync : (('a -> unit) -> 'a test) Core.identified
end

include module type of V1
