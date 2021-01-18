open Core
open Async_kernel
open Async_unix

module M = struct
  include Deferred

  let bind x f = bind x ~f

  let catch t on_error =
    try_with t >>= function Ok a -> return a | Error exn -> on_error exn
end

let run_test timeout name fn args =
  Clock.with_timeout timeout (fn args) >>| function
  | `Result x -> x
  | `Timeout ->
      Alcotest.fail
        (Printf.sprintf "%s timed out after %s" name
           (Time.Span.to_string_hum timeout))

module V1 = struct
  module Tester = Alcotest_engine.V1.Cli.Make (Alcotest.Unix_platform) (M)
  include Tester

  let test_case_sync n s f = test_case n s (fun x -> Deferred.return (f x))

  let test_case ?(timeout = sec 2.) name s f =
    test_case name s (run_test timeout name f)
end

include V1

(* module Unstable = struct
 *   module Tester = Alcotest_engine.Unstable.Cli.Make (Alcotest.Unix_platform) (M)
 *   include Tester
 * 
 *   let test_sync ?here ?tag ?tags ~name fn =
 *     test ?here ?tag ?tags ~name (fun x -> Deferred.return (fn x))
 * 
 *   let test ?here ?tag ?tags ~name ?(timeout = sec 2.) fn =
 *     test ?here ?tag ?tags ~name (run_test timeout name fn)
 * end *)
