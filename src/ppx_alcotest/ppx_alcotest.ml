open Ppxlib

module Promise_type = struct
  type t = Id | Lwt | Async

  let pp ppf = function
    | Id -> Fmt.pf ppf "Id"
    | Lwt -> Fmt.pf ppf "Lwt"
    | Async -> Fmt.pf ppf "Async"

  let of_extension_name = function
    | "test" -> Id
    | "test_lwt" -> Lwt
    | "test_async" -> Async
    | _ -> assert false

  let run_with ~loc ~runner test_type =
    match (runner, test_type) with
    | Id, Id -> [%expr Alcotest.Unstable.test]
    | Lwt, Id -> [%expr Alcotest_lwt.Unstable.test_sync]
    | Lwt, Lwt -> [%expr Alcotest_lwt.Unstable.test]
    | Async, Id -> [%expr Alcotest_async.Unstable.test_sync]
    | Async, Async -> [%expr Alcotest_async.Unstable.test]
    | a, b -> Fmt.failwith "internal error: trying to run %a on %a" pp b pp a

  let group_with ~loc ~runner =
    match runner with
    | Id -> [%expr Alcotest.Unstable.group]
    | Lwt -> [%expr Alcotest_lwt.Unstable.group]
    | Async -> [%expr Alcotest_async.Unstable.group]

  type ty = t

  module Set : sig
    type t

    val empty : t
    val add : t -> ty -> t
    val top_runner_exn : t -> ty
  end = struct
    type t = int

    let empty = 0
    let add t = function Id -> t lor 1 | Lwt -> t lor 2 | Async -> t lor 4

    let to_list t =
      (if t land 2 > 0 then [ Lwt ] else [])
      @ (if t land 4 > 0 then [ Async ] else [])
      @ if t land 1 > 0 then [ Id ] else []

    let top_runner_exn t =
      match to_list t with
      | [] -> Id (* No runner for empty test suite *)
      | [ Id ] -> Id
      | [ Lwt ] | [ Lwt; Id ] -> Lwt
      | [ Async ] | [ Async; Id ] -> Async
      | Lwt :: Async :: _ -> failwith "Can't combine test forms Lwt and Async"
      | _ -> assert false
  end
end

module type S = sig
  val check : core_type -> expression
  val here : expression

  val collect_tests : structure -> structure
  (** Expand [let%test], [module%test], and [%run_test] forms. *)
end

module Located (A : Ast_builder.S) : S = struct
  open A

  module Collection_site = struct
    let p = [%pat? _ppx_alcotest_suite]
    let e = [%expr _ppx_alcotest_suite]
    let add to_add = [%stri let [%p p] = [%e to_add] :: [%e e]]

    let from_module ~loc name =
      Ast_builder.Default.evar ~loc (name ^ "._ppx_alcotest_suite")
  end

  let collect_tests =
    [%expr Ppx_alcotest_runtime.list_rev [%e Collection_site.e]]

  let run_tests ~loc ~file_path : Promise_type.t -> expression = function
    | Id ->
        [%expr
          Alcotest.Unstable.run ~name:[%e estring file_path] [%e collect_tests]]
    | Lwt ->
        [%expr
          Lwt_main.run
            (Alcotest_lwt.Unstable.run ~name:[%e estring file_path]
               [%e collect_tests])]
    | Async -> failwith "TODO: do something"

  let collect_promise_types =
    object
      inherit [Promise_type.Set.t] Ast_traverse.fold as super

      method! structure_item t acc =
        match t with
        | {
         pstr_desc =
           Pstr_extension
             (({ txt = ("test" | "test_lwt" | "test_async") as name; _ }, _), _);
         _;
        } ->
            Promise_type.Set.add acc (Promise_type.of_extension_name name)
        | _ -> super#structure_item t acc
    end

  type fold_state = { test_found : bool }

  let collector file_path runner =
    object (collector)
      inherit [fold_state] Ast_traverse.fold_map as super

      method! expression t acc =
        match t with
        | [%expr [%collect_tests]] -> (collect_tests, acc)
        | [%expr [%run_tests]] ->
            (run_tests ~loc:t.pexp_loc ~file_path runner, acc)
        | t -> super#expression t acc

      method! structure t _ =
        match super#structure t { test_found = false } with
        | (_, { test_found = false }) as x -> x
        | items, ({ test_found = true } as acc) ->
            ([%stri let [%p Collection_site.p] = []] :: items, acc)

      method! structure_item t acc =
        match t with
        | [%stri [%%run_tests]] ->
            ( [%stri let () = [%e run_tests ~loc:t.pstr_loc ~file_path runner]],
              acc )
        | {
         pstr_desc =
           Pstr_extension
             ( ( {
                   txt = ("test" | "test_lwt" | "test_async") as extension_name;
                   _;
                 },
                 payload ),
               _attrs );
         _;
        } -> (
            let open Ast_builder.Default in
            match payload with
            (* [ module%test*? <module_name> = <body> ] *)
            | PStr
                [
                  {
                    pstr_desc =
                      Pstr_module
                        {
                          pmb_name = { txt = module_name; _ };
                          pmb_expr = body;
                          _;
                        };
                    pstr_loc = loc;
                  };
                ] ->
                let module_name = Option.get module_name in
                let item =
                  let name = Located.mk ~loc (Some module_name) in
                  let expr, _ =
                    collector#module_expr body { test_found = false }
                  in
                  [%stri
                    include struct
                      [%%i pstr_module ~loc (module_binding ~loc ~name ~expr)]

                      [%%i
                      Collection_site.add
                        [%expr
                          [%e Promise_type.group_with ~loc ~runner]
                            ~name:[%e estring ~loc module_name]
                            (Ppx_alcotest_runtime.list_rev
                               [%e Collection_site.from_module ~loc module_name])]]
                    end]
                in
                (item, { test_found = true })
            (* [ let%test*? <test_name> = <body> ] *)
            | PStr [ ([%stri let [%p? test_name] = [%e? body]] as item) ] ->
                let loc = item.pstr_loc in
                let test_name =
                  match test_name.ppat_desc with
                  | Ppat_constant (Pconst_string (s, _, _)) -> s
                  | Ppat_constant (Pconst_integer (s, _)) -> s
                  | _ -> Raise.invalid_test_name test_name
                in
                let testfn =
                  Promise_type.of_extension_name extension_name
                  |> Promise_type.run_with ~loc ~runner
                in
                let test =
                  [%stri
                    include struct
                      let [%p pvar ~loc ("_ppx_test__" ^ test_name)] =
                       fun _ (* TODO: replace with () once args worked out *) ->
                        [%e body]

                      [%%i
                      Collection_site.add
                        [%expr
                          [%e testfn] ~name:[%e estring ~loc test_name]
                            [%e evar ~loc ("_ppx_test__" ^ test_name)]]]
                    end]
                in
                (test, { test_found = true })
            | _ -> assert false)
        | { pstr_desc = Pstr_module _; _ } ->
            let x, _acc = super#structure_item t { test_found = false } in
            (x, { test_found = false })
        | _ -> super#structure_item t acc
    end

  let collect_tests s =
    let promise_types =
      collect_promise_types#structure s Promise_type.Set.empty
    in
    let runner = Promise_type.Set.top_runner_exn promise_types in
    let file_path = File_path.get_default_path_str s in
    fst ((collector file_path runner)#structure s { test_found = false })

  let here =
    let pos = loc.Location.loc_start in
    let id = Located.lident in
    pexp_record
      [
        (id "Lexing.pos_fname", estring pos.Lexing.pos_fname);
        (id "pos_lnum", eint pos.Lexing.pos_lnum);
        (id "pos_cnum", eint pos.Lexing.pos_cnum);
        (id "pos_bol", eint pos.Lexing.pos_bol);
      ]
      None

  let check typ =
    let testable =
      match Infer.core_type typ with
      | None -> failwith "bad"
      | Some x -> Infer.comb ~loc x
    in
    [%expr
      fun ?(here = [%e here]) ?msg ~expected actual ->
        Ppx_alcotest_runtime.check ~here ~testable:[%e testable] ?msg ~expected
          actual]
end

let test_collection =
  let (module A) = Ast_builder.make Location.none in
  let (module X) = (module Located (A) : S) in
  X.collect_tests

let extensions =
  let declare name ctx pat f =
    Extension.declare ("Alcotest." ^ name) ctx pat (fun ~loc ~path:_ ->
        let (module A) = Ast_builder.make loc in
        f (module Located (A) : S))
  in
  let module E = Extension.Context in
  let module A = Ast_pattern in
  [
    declare "check" E.expression A.(ptyp __) (fun (module X) -> X.check);
    declare "here" E.expression A.(pstr nil) (fun (module X) -> X.here);
  ]

let () =
  Driver.register_transformation ~extensions ~impl:test_collection "alcotest"
