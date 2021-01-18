(*
 * Copyright (c) 2013-2016 Thomas Gazagnaire <thomas@gazagnaire.org>
 * Copyright (c) 2020-2021 Craig Ferguson <me@craigfe.io>
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

include Core_intf
include Types
open! Import
open Model

let () =
  let print_error =
    (* We instantiate the error print buffer lazily, so as to be sensitive to
       [Fmt_tty.setup_std_outputs]. *)
    lazy
      (let buf = Buffer.create 0 in
       let ppf = Format.formatter_of_buffer buf in
       Fmt.set_style_renderer ppf Fmt.(style_renderer stderr);
       fun error ->
         Fmt.pf ppf "Alcotest assertion failure@.%a@." error ();
         let contents = Buffer.contents buf in
         Buffer.clear buf;
         contents)
  in
  Printexc.register_printer (function
    | Check_error err -> Some (Lazy.force print_error err)
    | _ -> None)

module Make (P : Platform.MAKER) (M : Monad.S) = struct
  module P = P (M)
  module M = Monad.Extend (M)

  module Suite = struct
    include Suite (M)

    let of_tests_exn ~name ~loc tests =
      match of_tests ~name ~loc tests with
      | Ok s -> s
      | Error (`Duplicate_path path) ->
          Pp.user_error "Duplicate test path: %s" path
      | Error `Empty_name ->
          Pp.user_error
            "Suite name cannot cannot be empty. Please pass a non-empty string \
             to `run`."
  end

  module Log_trap = Log_trap.Make (M) (P)

  module Pp = struct
    include Pp
    include Pp.Width_sensitive (P)
  end

  open M.Syntax

  module T = struct
    type 'a t = {
      suite : 'a Suite.t;
      loc : Lexing.position option;
      config : Config.t;
      run_id : string;
      log_trap : Log_trap.t;
      progress_reporter : Pp.Progress_reporter.t option;
      mutable errors : unit Fmt.t list;
    }

    let gen_run_id =
      let random_state = lazy (Random.State.make_self_init ()) in
      let random_hex _ =
        let state = Lazy.force random_state in
        match Random.State.int state 36 with
        | n when n < 10 -> Char.chr (n + Char.code '0')
        | n -> Char.chr (n - 10 + Char.code 'A')
      in
      fun () -> String.v ~len:8 random_hex

    let v ~suite ~loc ~config =
      let run_id = gen_run_id () in
      let log_trap =
        match config#verbose with
        | true -> Log_trap.inactive
        | false ->
            Log_trap.active ~root:config#log_dir ~uuid:run_id
              ~suite_name:(Suite.name suite)
      in
      let progress_reporter =
        match config#json with
        | true -> None
        | false ->
            Some
              (Pp.Progress_reporter.create ~ppf:Fmt.stdout
                 ~isatty:(P.stdout_isatty ()) ~compact:config#compact
                 ~selector_on_first_failure:
                   (not (config#verbose || config#show_errors)))
      in
      { suite; errors = []; loc; config; run_id; log_trap; progress_reporter }
  end

  open T

  type 'a test = 'a Suite.test

  let tagset ~tags ~tag =
    match (tags, tag) with
    | None, None -> Tag.Set.empty
    | None, Some t -> Tag.Set.(add t empty)
    | Some ts, None -> ts
    | Some ts, Some t -> Tag.Set.add t ts

  let bt () = match Printexc.get_backtrace () with "" -> "" | s -> "\n" ^ s

  let exn path name pp : Run_result.t =
    Exn (path, name, Fmt.(pp ++ const lines (bt ())))

  let test ?here ?tag ?tags ~name fn =
    let tags = tagset ~tags ~tag in
    Suite.test ~name ~tags ~loc:here fn

  let group ?here ?tag ?tags ~name ts =
    let tags = tagset ~tags ~tag in
    Suite.group ~name ~tags ~loc:here ts

  let pp_suite_results t =
    let log_dir = Log_trap.pp_current_run_dir t.log_trap in
    Pp.suite_results ~log_dir t.config

  let report_event t event =
    match t.progress_reporter with
    | None -> ()
    | Some pr -> Pp.Progress_reporter.event pr event

  let _pp_info _t = Pp.info
  let red ppf fmt = Fmt.kstrf (fun str -> Fmt.(styled `Red string) ppf str) fmt
  let map_pp_unit t ~f ppf = f (fun ppf () -> t ppf) ppf ()

  let pp_error t ppf (e : Run_result.t) =
    let (path, name), error_fmt =
      match e with
      | Error (p, f) -> (p, f)
      | Exn (p, _, f) -> (p, f)
      | _ -> assert false
    in
    let pp_logs ppf () =
      let pp_logs =
        Log_trap.recover_logs ~tail:t.config#tail_errors t.log_trap (path, name)
      in
      match (t.config#verbose, pp_logs) with
      | true, _ | _, None -> Fmt.pf ppf "%a@," error_fmt ()
      | false, Some pp_logs ->
          let pp_log_dir =
            map_pp_unit
              ~f:(fun s -> Pp.quoted (Fmt.styled `Cyan s))
              (Log_trap.pp_log_location t.log_trap (path, name))
          in
          Fmt.pf ppf "%t@,Logs saved to %t.@," pp_logs pp_log_dir
    in
    Fmt.(
      Pp.with_surrounding_box
        (const Pp.event_line { index = (path, name); type_ = Result e })
      ++ pp_logs
      ++ Pp.horizontal_rule
      ++ cut)
      ppf ()

  type running_state = {
    tests_so_far : int;
    first_error : int option;
    failures : int;
  }
  (** State that is kept during the test executions. *)

  let with_captured_logs t idx fn args =
    if t.config#verbose then fn args
    else
      Log_trap.with_captured_logs t.log_trap idx
        (fun () ->
          (* When capturing the logs of a test, also add the result of the test
             at the end. *)
          let+ result = fn args in
          Pp.rresult_error Fmt.stdout result;
          result)
        ()

  let protect_test :
        'a. Run_result.index -> ('a -> unit M.t) -> 'a -> Run_result.t M.t =
   fun path f args ->
    M.catch
      (fun () -> f args >|= fun () -> Run_result.Ok)
      ((function
         | Check_error err ->
             let err = Fmt.(err ++ const string (bt ())) in
             Run_result.Error (path, err)
         | Failure s -> exn path "failure" Fmt.(const string s)
         | Invalid_argument s -> exn path "invalid" Fmt.(const string s)
         | e -> exn path "exception" Fmt.(const exn e))
      >> M.return)

  let perform_test t args ~path ~(name : Safe_string.t) test acc =
    let test = protect_test (path, name) test in
    let* () = M.return () in
    report_event t { index = (path, name); type_ = Start };
    Fmt.(flush stdout) () (* Show event before any test stderr *);
    let+ (result : Run_result.t) =
      with_captured_logs t (path, name) test args
    in
    (* Store errors *)
    let errored : bool =
      let error, errored =
        if Run_result.is_failure result then
          ([ Fmt.const (pp_error t) result ], true)
        else ([], false)
      in
      t.errors <- error @ t.errors;
      errored
    in
    (* Show any remaining test output before the event *)
    Fmt.(flush stdout ());
    Fmt.(flush stderr ());
    report_event t { index = (path, name); type_ = Result result };
    let error = if errored then Some acc.tests_so_far else None in
    let acc =
      {
        tests_so_far = acc.tests_so_far + 1;
        first_error = Option.(acc.first_error || error);
        failures = (acc.failures + if errored then 1 else 0);
      }
    in
    (acc, result)

  exception Test_error

  let perform_tests : type a. a t -> a -> int M.t =
   fun t args ->
    let currently_bailing acc =
      Option.is_some acc.first_error && t.config#bail
    in
    let test_it = perform_test t args in
    let* () = M.return () in
    Fmt.pr "Testing %a.@,@[<v>%a@]"
      (Pp.quoted Fmt.(styled `Bold Suite.pp_name))
      t.suite
      Fmt.(
        styled `Faint (fun ppf () ->
            pf ppf "This run has ID %a.@,@," (Pp.quoted string) t.run_id))
      ();
    let start_time = P.time () in
    let+ results =
      Suite.foldi_until t.suite ~filter:t.config#filter
        ~init:{ tests_so_far = 0; first_error = None; failures = 0 }
        ~finish:(fun x -> x)
        ~group:(fun _ctx acc _ ->
          if currently_bailing acc then M.return (Stop acc)
          else M.return (Continue acc))
        ~test:(fun ctx (acc : running_state) test ->
          if currently_bailing acc then M.return (Stop acc)
          else
            match test with
            | `Run f ->
                let+ x, _ = test_it ~path:ctx#path ~name:ctx#name f acc in
                Continue x
            | `Skip ->
                report_event t
                  { index = (ctx#path, ctx#name); type_ = Result Skip };
                M.return (Continue acc))
    in
    let () =
      if currently_bailing results then
        match results.tests_so_far - Option.get_exn results.first_error - 1 with
        | n when n > 0 ->
            Fmt.pr "@\n  %a@\n"
              Fmt.(styled `Faint string)
              (Fmt.str "... with %d subsequent test%a skipped." n Pp.plural n)
        | 0 -> ()
        | _ -> assert false
    in
    let time = P.time () -. start_time
    and success = results.tests_so_far
    and failures = results.failures
    and errors = List.rev t.errors in
    (pp_suite_results t) Fmt.stdout Pp.{ time; success; failures; errors };
    failures

  let list_tests ?here ?config:_ ~name tests =
    let suite = Suite.of_tests_exn ~name ~loc:here tests in
    Fmt.pr "@[<v>%a@]@." Suite.pp suite;
    M.return ()

  let default_log_dir =
    let ( / ) = Filename.concat in
    lazy (P.getcwd () / "_build" / "_tests")

  let run_with_args ?here ?(config = Config.User.create ()) ~name test_ctx tests
      =
    let config = Config.apply_defaults ~default_log_dir config in
    let suite = Suite.of_tests_exn ~name ~loc:here tests in
    let* at_least_one_test =
      Suite.foldi_until suite ~filter:config#filter ~init:()
        ~finish:(fun () -> false)
        ~test:
          (fun _ () -> function
            | `Run _ -> M.return (Stop true)
            | `Skip -> M.return (Continue ()))
    in
    if not at_least_one_test then (
      Fmt.(pf stderr)
        "%a\n" red
        "Invalid request (no tests to run, filter skipped everything)!";
      exit 1);
    let t = T.v ~suite ~loc:here ~config in
    let+ test_failures = perform_tests t test_ctx in
    match (test_failures, config#and_exit) with
    | 0, true -> exit 0
    | 0, false -> ()
    | _, true -> exit 1
    | _, false -> raise Test_error

  let run ?here ?config ~name suite = run_with_args ?here ?config ~name () suite
end

module Make_v1 : V1_types.MAKER =
functor
  (P : Platform.MAKER)
  (M : Monad.S)
  ->
  struct
    module X = Make (P) (M)
    module P = P (M)
    module M = Monad.Extend (M)
    open M.Syntax

    (* Types *)
    type return = unit M.t
    type 'a run = 'a -> unit M.t
    type speed_level = [ `Quick | `Slow ]

    exception Test_error

    type 'a test_case = string * speed_level * 'a run

    let test_case n s f = (n, s, f)

    type 'a test = string * 'a test_case list

    type 'a with_options =
      ?and_exit:bool ->
      ?verbose:bool ->
      ?compact:bool ->
      ?tail_errors:[ `Unlimited | `Limit of int ] ->
      ?quick_only:bool ->
      ?show_errors:bool ->
      ?json:bool ->
      ?filter:(name:string -> index:int -> [ `Run | `Skip ]) ->
      ?log_dir:string ->
      ?bail:bool ->
      'a

    let migrate_suite ts =
      ListLabels.map ts ~f:(fun (group_name, children) ->
          let children =
            ListLabels.mapi children ~f:(fun i (name, speed_level, fn) ->
                let speed =
                  match speed_level with
                  | `Quick -> Tag.Speed_level.quick
                  | `Slow -> Tag.Speed_level.slow
                in
                let position = Tag.V (Tag.Position.tag, (group_name, i)) in
                let tags = Tag.Set.(empty |> add speed |> add position) in
                X.test ~name ~tags fn)
          in
          X.group ~name:group_name children)

    let run_main config name args tl =
      let config = Config.User.(create () || config) in
      let suite = migrate_suite tl in
      X.run_with_args ~name ~config args suite >|= fun _ -> exit 0

    let list_tests name (ts : _ test list) =
      let suite = migrate_suite ts in
      X.list_tests ~name suite

    let run' config name (tl : unit test list) = run_main config name () tl

    let run_with_args ?and_exit ?verbose ?compact ?tail_errors ?quick_only
        ?show_errors ?json ?filter ?log_dir ?bail =
      Config.User.kcreate run_main ?and_exit ?verbose ?compact ?tail_errors
        ?quick_only ?show_errors ?json
        ?filter:(Option.map (fun f -> `V1 f) filter)
        ?log_dir ?bail

    let run : (string -> unit test list -> return) with_options =
     fun ?and_exit ?verbose ?compact ?tail_errors ?quick_only ?show_errors ?json
         ?filter ?log_dir ?bail ->
      Config.User.kcreate run' ?and_exit ?verbose ?compact ?tail_errors
        ?quick_only ?show_errors ?json
        ?filter:(Option.map (fun f -> `V1 f) filter)
        ?log_dir ?bail

    let run' = run'
    let run_with_args' a b c d = run_main a b c d
  end

module V1 = struct
  include V1_types
  module Make = Make_v1
end

module Unstable = struct
  type nonrec 'a identified = 'a identified

  include Unstable_types
  module Make = Make
end
