open! Import

module Index = struct
  type t =
    | Root
    | Child of {
        parent_path : Safe_string.t list;
        name : Safe_string.t;
        last : bool;
      }

  let root = Root

  let rcons t ~last ~name =
    let parent_path =
      match t with
      | Root -> []
      | Child { parent_path; name; _ } -> parent_path @ [ name ]
    in
    Child { parent_path; name; last }

  let last_in_parent = function Root -> false | Child { last; _ } -> last

  let parent_path = function
    | Root -> failwith "Root node has no parent"
    | Child { parent_path; _ } -> parent_path

  let leaf_name = function
    | Root -> failwith "Root node has no name"
    | Child { name; _ } -> name
end

module Run_result = struct
  type t =
    | Ok
    | Exn of Index.t * string * unit Fmt.t
    | Error of Index.t * unit Fmt.t
    | Skip
    | Todo of string

  let is_failure = function
    | Ok | Skip -> false
    | Error _ | Exn _ | Todo _ -> true

  let has_run = function Ok | Error _ | Exn _ -> true | Skip | Todo _ -> false
end

module Suite (M : Monad.S) = struct
  module String_set = Set.Make (String)
  module M = Monad.Extend (M)
  open M.Syntax

  type 'a test =
    | Test of {
        name : Safe_string.t;
        loc : Lexing.position option;
        tags : Tag.Set.t;
        fn : 'a -> unit M.t;
      }
    | Group of {
        name : Safe_string.t;
        loc : Lexing.position option;
        tags : Tag.Set.t;
        children : 'a test list;
      }

  let test ~name ~loc ~tags fn =
    let name = Safe_string.v name in
    Test { name; loc; tags; fn }

  let group ~name ~loc ~tags children =
    let name = Safe_string.v name in
    Group { name; loc; tags; children }

  type 'a t = {
    name : Safe_string.t;
    loc : Lexing.position option;
    tests : 'a test list;
  }

  let validate_tests =
    let rec aux ctx ts =
      let open Result.Syntax in
      let* () =
        let duplicate =
          ts
          |> List.map (function
               | Test { name; _ } -> name
               | Group { name; _ } -> name)
          |> List.find_duplicate ~compare:Safe_string.compare
        in
        match duplicate with
        | None -> Ok ()
        | Some dup ->
            let path =
              List.rev_map Safe_string.to_string (dup :: ctx)
              |> String.concat ~sep:" › "
            in
            Error (`Duplicate_path path)
      in
      List.fold_result ts ~init:() ~f:(fun () -> function
        | Test _ -> Ok ()
        | Group { name; children; _ } -> aux (name :: ctx) children)
    in
    fun ts -> aux [] ts

  let of_tests ~name ~loc tests =
    let open Result.Syntax in
    let* () = validate_tests tests in
    if String.is_empty name then Error `Empty_name
    else
      let name = Safe_string.v name in
      Ok { name; loc; tests }

  let rec list_fold_until ~f ~init:acc ~finish = function
    | [] -> M.return (finish acc)
    | [ x ] -> (
        f ~last:true acc x >|= function Stop c -> c | Continue c -> finish c)
    | x :: (_ :: _ as xs) -> (
        f ~last:false acc x >>= function
        | Stop c -> M.return c
        | Continue acc -> list_fold_until ~f ~init:acc ~finish xs)

  let foldi_until ~filter ?group ?test ~init:acc ~finish t =
    let fold_default _ acc _ = M.return (Continue acc) in
    let group = Option.value group ~default:fold_default
    and test = Option.value test ~default:fold_default in
    let rec aux ~last ~index acc elt =
      match elt with
      | Test t ->
          let index = Index.rcons index ~last ~name:t.name in
          let arg =
            match filter t.tags with `Run -> `Run t.fn | `Skip -> `Skip
          in
          test index acc arg
      | Group g -> (
          let index = Index.rcons index ~last ~name:g.name in
          let arg = filter g.tags in
          M.bind (group index acc arg) @@ function
          | Stop _ as x -> M.return x
          | Continue acc -> (
              match arg with
              | `Skip -> M.return (Continue acc)
              | `Run ->
                  list_fold_until g.children ~init:acc
                    ~f:(fun ~last acc child ->
                      aux ~last ~index acc child >|= function
                      | Continue _ as c -> c
                      | Stop _ as s -> Stop s)
                    ~finish:(fun x -> Continue x)))
    in
    list_fold_until t.tests ~init:acc ~finish ~f:(aux ~index:Index.root)

  let fold ~filter ~group ~test ~init t =
    foldi_until t ~filter ~init
      ~finish:(fun x -> x)
      ~group:(fun _ a x -> M.return (Continue (group a x)))
      ~test:(fun _ a x -> M.return (Continue (test a x)))

  let name { name; _ } = Safe_string.to_string name
  let pp_name ppf { name; _ } = Safe_string.pp ppf name

  let rec pp_files ~pre ~last_dir ppf files =
    let open Fmt in
    (* Only print newline at the end if our ancestors have not already done so
       (i.e. we are not the descendant of a last directory *)
    let pp_last_dir ppf last_dir = if not last_dir then pf ppf "@,%s" pre in
    let pp_children_last ppf =
      pf ppf "%s└─ %a" pre (pp_file ~last_dir:true ~pre:(pre ^ "   "))
    and pp_children_not_last ppf =
      pf ppf "%s├─ %a" pre (pp_file ~last_dir:false ~pre:(pre ^ "│  "))
    in
    match files with
    | [] -> ()
    | [ last ] -> pf ppf "@,%a%a" pp_children_last last pp_last_dir last_dir
    | _ :: _ :: _ ->
        let last, not_last =
          match List.rev files with
          | [] -> assert false
          | last :: not_last_rev -> (last, List.rev not_last_rev)
        in
        pf ppf "@,%a@,%a%a"
          (list ~sep:cut pp_children_not_last)
          not_last pp_children_last last pp_last_dir last_dir

  and pp_file ~pre ~last_dir ppf =
    let open Fmt in
    let pp_group_name = styled `Bold (styled `Blue Safe_string.pp) in
    function
    | Test { name; tags; _ } ->
        Fmt.pf ppf "%a  %a" Safe_string.pp name Fmt.(styled `Faint pp_tags) tags
    | Group { name; children; _ } ->
        pp_group_name ppf name;
        pp_files ~pre ~last_dir ppf children

  and pp_tags ppf ts =
    let open Fmt in
    string ppf "< ";
    list ~sep:(const string "; ") Tag.pp ppf (Tag.Set.to_list ts);
    Fmt.string ppf " >"

  let pp_file ppf t = pp_file ~pre:"" ~last_dir:false ppf t

  let pp ppf t =
    pp_file ppf
      (Group
         { name = t.name; loc = None; tags = Tag.Set.empty; children = t.tests })
end
