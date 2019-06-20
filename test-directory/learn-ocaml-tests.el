;; -*- lexical-binding: t; -*-

(require 'ert-async)
(require 'learn-ocaml)

(ert-deftest-async learn-ocaml-download-server-file-test (done)
  (shell-command "rm -f demo.ml")
  (let ((test (lambda(callback)
		(learn-ocaml-download-server-file
		 :callback (lambda (s)
			     (should (= (shell-command "cat demo.ml") 0))
			     (funcall done))
		 :id "demo"))))
    (funcall test done)))
  
  
