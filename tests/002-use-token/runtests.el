;;; runtests.el --- unit tests -*- lexical-binding: t; -*-

;; Copyright (c) 2019-2021  The learn-ocaml.el developers

;; This software is free software; you can redistribute it and/or
;; modify it under the terms of the MIT License.

;; You should have received a copy of the MIT License along with this
;; program.  If not, see <https://spdx.org/licenses/MIT>

;;; Commentary:
;;
;; This file contains "common" tests, that should always be run.
;;

;;;  Eval these lines to run the tests interactively <C-x C-e>
;;
;; (progn (load-file "../../learn-ocaml.el") (load-file "../learn-ocaml-tests.el") (load-file "./runtests.el"))
;; (add-to-list 'exec-path (learn-ocaml-test-client-expected-path))
;; (progn (learn-ocaml-test-use-passwd-auto)(learn-ocaml-test-dir))
;; (call-interactively #'ert-run-tests-interactively)

;; (setq debug-on-error t)  ; to open the debugger/backtrace on error

;;; Code:

;; Tests for core functions

(ert-deftest-async-map-symb
  2_learn-ocaml-token-management-test (login-teacher signup) action (done)
  (learn-ocaml-test-run-with
   :before-action action
   :body (lambda ()
           (learn-ocaml-create-token-cmd
            "Foo"
            ""
            (lambda (token)
              (learn-ocaml-use-metadata-cmd
               token
               nil
               (lambda (_)
                 (learn-ocaml-give-token-cmd
                  (lambda (given_token)
                    (should
                     (string-equal
                      given_token
                      token ))
                    (funcall done))))))))))

(ert-deftest-async-map-symb
  8_learn-ocaml-init-another-token (login-teacher signup) action (done)
  (learn-ocaml-test-run-with
   :before-action action
   :body (lambda ()
           (learn-ocaml-create-token-cmd
            "Foo"
            ""
            (lambda (token)
              (learn-ocaml-init
               :new-server-value nil
               :new-token-value token
               :callback (lambda (_)
                           (learn-ocaml-give-token-cmd
                            (lambda (token2)
                              (should (equal token token2))
                              (funcall done))))))))))

(ert-deftest-async-map-symb
  9_learn-ocaml-init-create-token (login-teacher signup) action (done)
  (learn-ocaml-test-run-with
   :before-action action
   :body (lambda ()
           (learn-ocaml-give-token-cmd
            (lambda (previous-token)
              (learn-ocaml-init
               :new-server-value nil
               :nickname "Foo"
               :secret ""
               :callback (lambda (_)
                           (learn-ocaml-give-token-cmd
                            (lambda (token2)
                              (should-not (equal previous-token token2))
                              (funcall done))))))))))

;; tests "without" the config file

;; MOVED TO 001-common:
;; (ert-deftest-async a10_learn-ocaml-on-load-test-another-token-no-config (done)
;; ...

(ert-deftest-async a11_learn-ocaml-on-load-test-create-token-no-config (done)
  (learn-ocaml-test-run-with
   ;; No :before-action 'login-teacher/'signup; just pre-teardown
   :body (lambda ()
           (learn-ocaml-init
            :new-server-value learn-ocaml-test-url
            :nickname "Foo"
            :secret ""
            :callback (lambda (_)
                        (learn-ocaml-give-token-cmd
                         (lambda (token2)
                           (funcall done))))))))

;;; runtests.el ends here
