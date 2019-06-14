;;Testing give-token giver-server and use-metadata
(defconst test-server "http://localhost")

(defun fixture ()
  (shell-command "rm ~/.config/learnocaml/client.json")
)
(ert-deftest use-metadata-test ()
 (learn-ocaml-use-metadata "X-WRL-UBA-YOM-STO" 
 (learn-ocaml-give-token
)
