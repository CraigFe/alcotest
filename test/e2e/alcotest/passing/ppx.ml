module%test Lorem = struct
  let%test "ipsum" = ()
  let%test "dolor" = ()
end

module%test Sit = struct
  module%test Amet = struct
    let%test "consectetur" = ()
  end

  let%test "adipiscing" = ()
end

let () = print_endline ""

[%%run_tests]
