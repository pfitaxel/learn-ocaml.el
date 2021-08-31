;;; runtests.el --- unit tests -*- lexical-binding: t; -*-

;; Copyright (c) 2019-2021  The learn-ocaml.el developers

;; This software is free software; you can redistribute it and/or
;; modify it under the terms of the MIT License.

;; You should have received a copy of the MIT License along with this
;; program.  If not, see <https://spdx.org/licenses/MIT>

;;; Commentary:
;;
;; This file contains tests that should only be run if USE_PASSWD=true
;; which implies 'learn-ocaml --version >= 0.14'.
;;

;;;  Eval these lines to run the tests interactively <C-x C-e>
;;
;; (progn (load-file "../../learn-ocaml.el") (load-file "../learn-ocaml-tests.el") (load-file "./runtests.el"))
;; (add-to-list 'exec-path (learn-ocaml-test-client-expected-path))
;; (progn (learn-ocaml-test-use-passwd-auto)(learn-ocaml-test-dir))
;; (call-interactively #'ert-run-tests-interactively)

;; (setq debug-on-error t)  ; to open the debugger/backtrace on error

;;; Code:

;; Tests for "use_passwd: true"

(ert-deftest-async a12_learn-ocaml-test-sign-up (done)
  (learn-ocaml-test-run-with
   ;; As we focus on sign-up,
   ;; no :before-action 'login-teacher/'signup; just pre-teardown
   :body (lambda ()
           (let ((email (learn-ocaml-test-user-email))
                 (pass (learn-ocaml-test-user-pass)))
             (learn-ocaml-client-sign-up-cmd
              :server learn-ocaml-test-url
              :login email
              :password pass
              :nickname (format "FooStudentWithEmail(%s)" email)
              :secret (learn-ocaml-test-secret)
              :callback-err
              (lambda (output) (error "a12_learn-ocaml-test-sign-up: failed with [%s]." output))
              :callback-ok
              (lambda (output)
                (should (string-match-p "confirmation e-mail" output))
                (funcall done)))))))

;;; runtests.el ends here
