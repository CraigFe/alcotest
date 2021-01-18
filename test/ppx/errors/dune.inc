(env
 (_
  (env-vars
   (OCAML_ERROR_STYLE "short")
   (OCAML_COLOR "never"))))

; -------- Test: `bad_test_payload.ml` --------



; Run the PPX on the `.ml` file
(rule
 (targets bad_test_payload.actual)
 (deps
  (:pp ../pp.exe)
  (:input bad_test_payload.ml))
 (action
  ; expect the process to fail, capturing stderr
  (with-stderr-to
   %{targets}
   (bash "! %{pp} -no-color --impl %{input}"))))

; Compare the post-processed output to the .expected file
(rule
 (alias runtest)
 (package ppx_alcotest)
 (action
  (diff bad_test_payload.expected bad_test_payload.actual)))


