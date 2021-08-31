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

(ert-deftest-async 1_learn-ocaml-server-management-test (done)
  (learn-ocaml-test-run-with
   :before-action 'login-teacher
   ;; TODO should also test with :before-signup t
   :body (lambda ()
           (learn-ocaml-use-metadata-cmd
            nil
            learn-ocaml-test-url
            (lambda (_)
              (learn-ocaml-give-server-cmd
               (lambda (given-server)
                 (should (string-equal
                          learn-ocaml-test-url
                          given-server))
                 (funcall done))))))))

(ert-deftest-async 3_learn-ocaml-grade-test (done)
  (learn-ocaml-test-run-with
   :before-action 'login-teacher
   :body (lambda ()
           (learn-ocaml-test-remove-temp-file "demo")
           (learn-ocaml-grade-file-cmd
            :id "demo"
            :file learn-ocaml-test-tograde-file
            :callback (lambda (_)
                        (should (= (shell-command
                                    (concat
                                     "cat "
                                     (learn-ocaml-temp-html-file "demo")
                                     " | grep \"Exercise complete\"")
                                    )
                                   0))
                        (learn-ocaml-test-remove-temp-file "demo")
                 (funcall done))))))

(ert-deftest-async 4_learn-ocaml-download-server-file-test (done)
  (learn-ocaml-test-run-with
   :before-action 'login-teacher
   :body (lambda ()
           (learn-ocaml-test-remove-demo-file)
           (learn-ocaml-download-server-file-cmd
            :id "demo"
            :directory learn-ocaml-fixture-directory
            :callback (lambda (s)
                        (should (= 0 (shell-command
                                      (concat "cat "
                                              learn-ocaml-test-demo-file))))
                        (learn-ocaml-test-remove-demo-file t)
                        (funcall done))))))

(ert-deftest-async 5_learn-ocaml-download-template-test (done)
  (learn-ocaml-test-run-with
   :before-action 'login-teacher
   :body (lambda ()
           (learn-ocaml-test-remove-demo-file)
           (learn-ocaml-download-template-cmd
            :id "demo"
            :directory learn-ocaml-fixture-directory
            :callback (lambda (s)
                        (should
                         (= 0 (shell-command
                               (concat "diff "
                                       learn-ocaml-test-demo-file
                                       " "
                                       learn-ocaml-test-template-file))))
                        (learn-ocaml-test-remove-demo-file t)
                        (funcall done))))))

(ert-deftest-async 6_learn-ocaml-give-exercise-list-test (done)
  (learn-ocaml-test-run-with
   :before-action 'login-teacher
   :body (lambda ()
           (with-temp-buffer
             (insert-file-contents learn-ocaml-test-json-file)
             (let ((expected (json-read-from-string (buffer-string))))
               (learn-ocaml-give-exercise-list-cmd
                (lambda (json)
                  (should (equal json expected))
                  (funcall done))))))))

(ert-deftest-async 7_learn-ocaml-compute-questions-url-test (done)
  (learn-ocaml-test-run-with
   :before-action 'login-teacher
   :body (lambda ()
           (learn-ocaml-give-server-cmd
            (lambda (server)
              (learn-ocaml-give-token-cmd
               (lambda (token)
                 (with-temp-buffer
                   (insert-file-contents learn-ocaml-test-description-file)
                   (let* ((url (learn-ocaml-compute-questions-url server "demo" token))
                          (expected (learn-ocaml-test-collapse-whitespace
                                     (buffer-string)))
                          (result (learn-ocaml-test-collapse-whitespace
                                   (shell-command-to-string (concat "curl -fsS " url )))))
                     (should (string-match expected result))))
                 (funcall done))))))))

;; tests "without" the config file

(ert-deftest-async a10_learn-ocaml-on-load-test-another-token-no-config (done)
  (learn-ocaml-test-run-with
   :before-action 'login-teacher
   :body (lambda ()
           (learn-ocaml-give-token-cmd
            (lambda (token)
              (learn-ocaml-test-remove-client-file)
              (learn-ocaml-init
               :new-server-value learn-ocaml-test-url
               :new-token-value token
               :callback (lambda (_)
                           (learn-ocaml-give-token-cmd
                            (lambda (token2)
                              (should (equal token token2))
                              (funcall done))))))))))

;; misc tests

(setq example-file shell-file-name) ; just to get a filename example

(ert-deftest a13_learn-ocaml-file-path ()
  (let* ((path example-file)
         (dir (file-name-directory path))
         (file (file-name-nondirectory path)))
    (should (string-equal (learn-ocaml-file-path (directory-file-name dir) file) path))
    (should (string-equal (learn-ocaml-file-path dir file) path))
    (should (string-equal (learn-ocaml-file-path "/dummy" path) path))))

;;; runtests.el ends here
