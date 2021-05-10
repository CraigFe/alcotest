let check :
      'a.
      here:Lexing.position ->
      ?msg:string ->
      testable:'a Alcotest.testable ->
      expected:'a ->
      'a ->
      unit =
 fun ~here ?(msg = "") ~testable ~expected actual ->
  Alcotest.check ~here testable msg expected actual

let run ~name a = Alcotest.Unstable.run ~name a
let test ~name a = Alcotest.Unstable.test ~name a
let group ~name a = Alcotest.Unstable.group ~name a
let list_rev = List.rev
