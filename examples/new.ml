(* A module with functions to test *)
module To_test = struct
  let lowercase = String.lowercase_ascii
  let capitalize = String.capitalize_ascii
  let str_concat = String.concat ""
  let list_concat = List.append
end

(* The tests *)
let test_lowercase () =
  Alcotest.(check string) "same string" "hello!" (To_test.lowercase "hELLO!")

let test_capitalize () =
  Alcotest.(check string) "same string" "World." (To_test.capitalize "world.")

let test_str_concat () =
  Alcotest.(check string)
    "same string" "foobar"
    (To_test.str_concat [ "foo"; "bar" ])

let test_list_concat () =
  Alcotest.(check (list int))
    "same lists" [ 1; 2; 3 ]
    (To_test.list_concat [ 1 ] [ 2; 3 ])

let () =
  let open Alcotest.Unstable in
  print_endline "";
  run
    ~config:(Config.v ~and_exit:false ())
    ~name:__FILE__
    [
      group ~name:"string-case"
        [
          test ~name:"Lower case" test_lowercase;
          group ~name:"Further_nested"
            [
              test ~name:"alpha" test_lowercase;
              test ~name:"beta" test_lowercase;
            ];
          test ~name:"Capitalization" test_capitalize;
        ];
      group ~name:"string-concat"
        [ test ~name:"String mashing" test_str_concat ];
      group ~name:"list-concat" [ test ~name:"List mashing" test_list_concat ];
    ]
