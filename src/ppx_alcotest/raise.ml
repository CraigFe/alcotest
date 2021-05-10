open Ppxlib

let invalid_test_name pattern =
  Location.raise_errorf ~loc:pattern.ppat_loc
    "Unsupported test name type. Use a literal string or an integer."
