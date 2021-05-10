; -------- Test: `alias.ml` --------

; The PPX-dependent executable under test
(executable
 (name alias)
 (modules alias)
 (preprocess (pps ppx_alcotest))
 (libraries alcotest))

; Run the PPX on the `.ml` file
(rule
 (targets alias.actual)
 (deps
  (:pp ../pp.exe)
  (:input alias.ml))
 (action
  (run %{pp} --impl %{input} -o %{targets})))

; Compare the post-processed output to the .expected file
(rule
 (alias runtest)
 (package ppx_alcotest)
 (action
  (diff alias.expected alias.actual)))

; Ensure that the post-processed executable runs correctly
(rule
 (alias runtest)
 (package ppx_alcotest)
 (action
  (run ./alias.exe)))


