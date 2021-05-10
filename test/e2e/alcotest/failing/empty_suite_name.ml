module Alcotest = Alcotest.V1

let () =
  Alcotest.run ""
    [ ("alpha", [ Alcotest.test_case "1" `Quick (fun () -> assert false) ]) ]
