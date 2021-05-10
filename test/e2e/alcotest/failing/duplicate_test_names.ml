(** Ensure that suites with duplicate test names are rejected. *)

module Alcotest = Alcotest.V1

let () =
  Alcotest.run __FILE__
    [
      ("duped", [ Alcotest.test_case "1" `Quick (fun () -> assert false) ]);
      ("duped", [ Alcotest.test_case "2" `Quick (fun () -> assert false) ]);
    ]
