module Alcotest = Alcotest.V1

let pid = Unix.getpid ()

let read_all_lines ic =
  let acc = ref [] in
  try
    while true do
      acc := input_line ic :: !acc
    done;
    assert false
  with End_of_file -> String.concat "\n" !acc

let test_case n () =
  let open_fds =
    let ic =
      Unix.open_process_in ("lsof -p " ^ string_of_int pid ^ " 2>/dev/null")
    in
    let t = read_all_lines ic in
    close_in ic;
    t
  in
  Printf.printf "Test #%d. File descriptors open: %s\n%!" n open_fds

let () =
  Alcotest.run __FILE__
    [
      ( "alpha",
        List.init 10 (fun i ->
            Alcotest.test_case (string_of_int i) `Quick (test_case i)) );
    ]
