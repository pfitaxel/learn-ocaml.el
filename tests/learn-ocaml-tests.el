;;; learn-ocaml-tests.el --- unit tests -*- lexical-binding: t; -*-

;; Copyright (c) 2019-2021  The learn-ocaml.el developers

;; This software is free software; you can redistribute it and/or
;; modify it under the terms of the MIT License.

;; You should have received a copy of the MIT License along with this
;; program.  If not, see <https://spdx.org/licenses/MIT>

;;; Commentary:
;;

;;;  Eval these lines to run the tests interactively <C-x C-e>
;;
;; (progn (load-file "../learn-ocaml.el") (load-file "learn-ocaml-tests.el"))
;; (call-interactively #'ert-run-tests-interactively)

;; (setq debug-on-error t)  ; to open the debugger/backtrace on error

(require 'learn-ocaml)
;;; Code:

(setq learn-ocaml-fail-noisely t)

(require 'ert-async)
;(setq ert-async-timeout 2)

;; NOTE: This symbol list gather tests specific to 'use_passwd: true'
(setq learn-ocaml-test-use-passwd-list
      '(a12_learn-ocaml-test-sign-up  2_learn-ocaml-token-management-test))

(setq learn-ocaml-test-skip-use-passwd
      `(not (member ,@learn-ocaml-test-use-passwd-list)))

;; WARNING: several tests delete the ./demo.ml and client.json files:
(setq learn-ocaml-test-client-file "~/.config/learnocaml/client.json")

(setq learn-ocaml-test-tograde-file (expand-file-name "to_grade.ml"))
(setq learn-ocaml-test-template-file (expand-file-name "template_demo.ml"))
(setq learn-ocaml-test-json-file (expand-file-name "exercise_list.json"))
(setq learn-ocaml-test-description-file (expand-file-name "expected_description.html"))

;; This fixture is needed because of Travis CI's permission mismatch:
;; bind-mount:'uid=2000(travis)' vs. current-user:uid=1000(learn-ocaml)'.
;; The function `learn-ocaml-temp-dir' auto-creates a temp directory.
(setq learn-ocaml-fixture-directory (learn-ocaml-temp-dir))
(setq learn-ocaml-test-demo-file
      (learn-ocaml-file-path learn-ocaml-fixture-directory "demo.ml"))

;; REMARK: unless otherwise noted, the tests assume that we have previously run
;; $ learn-ocaml-client init --server=http://localhost:8080 test test
(setq learn-ocaml-test-url "http://localhost:8080")

;; REMARK: some test also relies on the "curl" binary

(defun learn-ocaml-test-remove-demo-file (&optional shouldexist)
  (if shouldexist
      (shell-command (concat "rm " learn-ocaml-test-demo-file))
    (shell-command (concat "rm -f " learn-ocaml-test-demo-file))))

(defun learn-ocaml-test-remove-client-file ()
  (shell-command (concat "rm -f " learn-ocaml-test-client-file)))

(defun learn-ocaml-test-remove-temp-file (&optional id)
  (let ((file (learn-ocaml-temp-html-file id)))
    (shell-command (concat "rm -f " file))))

(defun learn-ocaml-test-collapse-whitespace (str)
  (replace-regexp-in-string "[[:space:]\n]+" " " str))

;; Tests for core functions

(ert-deftest-async 1_learn-ocaml-server-management-test (done)
  (let ((tests (lambda (callback)
         (learn-ocaml-use-metadata-cmd
          nil
          learn-ocaml-test-url
          (lambda (_)
            (learn-ocaml-give-server-cmd
           (lambda (given-server)
             (should (string-equal
                  learn-ocaml-test-url
                  given-server))
             (funcall callback))))))))
    (funcall tests done)))

(ert-deftest-async 2_learn-ocaml-token-management-test (done)
  (let ((tests (lambda (callback)
         (learn-ocaml-create-token-cmd
          "test"
          "test"
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
               (funcall callback))))))))))
    (funcall tests done)))


(ert-deftest-async 3_learn-ocaml-grade-test (done)
  (learn-ocaml-test-remove-temp-file "demo")
  (let ((test (lambda(callback)
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
                 (funcall callback))))))
    (funcall test done)))

(ert-deftest-async 4_learn-ocaml-download-server-file-test (done)
  (learn-ocaml-test-remove-demo-file)
  (let ((test (lambda(callback)
        (learn-ocaml-download-server-file-cmd
                 :id "demo"
                 :directory learn-ocaml-fixture-directory
         :callback (lambda (s)
                 (should (= 0 (shell-command
                       (concat "cat "
                                                   learn-ocaml-test-demo-file))))
                 (learn-ocaml-test-remove-demo-file t)
                 (funcall callback))))))
    (funcall test done)))

(ert-deftest-async 5_learn-ocaml-download-template-test (done)
  (learn-ocaml-test-remove-demo-file)
  (let ((test (lambda (callback)
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
                  (funcall callback))))))
    (funcall test done)))


(ert-deftest-async 6_learn-ocaml-give-exercise-list-test (done)
  (let ((test (lambda (callback)
        (with-temp-buffer
          (insert-file-contents learn-ocaml-test-json-file)
          (let ((expected (json-read-from-string (buffer-string))))
          (learn-ocaml-give-exercise-list-cmd
           (lambda (json)
             (should (equal json expected))
             (funcall callback))))))))
  (funcall test done)))


(ert-deftest-async 7_learn-ocaml-compute-questions-url-test (done)
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
          (funcall done))))))


(ert-deftest-async 8_learn-ocaml-init-another-token (done)
  (learn-ocaml-create-token-cmd
   "test"
   "test"
   (lambda (token)
     (learn-ocaml-init
      :new-server-value nil
      :new-token-value token
      :callback (lambda (_)
          (learn-ocaml-give-token-cmd
           (lambda (token2)
             (should (equal token token2))
             (funcall done))))))))

(ert-deftest-async 9_learn-ocaml-init-create-token (done)
  (learn-ocaml-give-token-cmd
   (lambda (previous-token)
     (learn-ocaml-init
      :new-server-value nil
      :nickname "test"
      :secret "test"
      :callback (lambda (_)
          (learn-ocaml-give-token-cmd
           (lambda (token2)
             (should-not (equal previous-token token2))
             (funcall done))))))))

;; tests without the config file

(ert-deftest-async a10_learn-ocaml-on-load-test-another-token-no-config (done)
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
                     ; (learn-ocaml-test-remove-client-file)
             (funcall done))))))))

(ert-deftest-async a11_learn-ocaml-on-load-test-create-token-no-config (done)
  (learn-ocaml-test-remove-client-file)
  (learn-ocaml-init
      :new-server-value learn-ocaml-test-url
      :nickname "test"
      :secret "test"
      :callback (lambda (_)
          (learn-ocaml-give-token-cmd
           (lambda (token2)
                     ; (learn-ocaml-test-remove-client-file)
             (funcall done))))))

(ert-deftest-async a12_learn-ocaml-test-sign-up (done)
 ; FIXME: (learn-ocaml-client-init-server-cmd learn-ocaml-test-url)
 (let* ((result (learn-ocaml-client-sign-up-cmd
                 learn-ocaml-test-url "ErtTest@example.com" "Ocaml123*" "Test"
                 (escape-secret ""))))
          (should (string-equal "A confirmation e-mail has been sent to your address.\nPlease go to your mailbox to finish creating your account,\n then you will be able to sign in.\n"
                                result)))
 (funcall done))

;; misc tests

(setq example-file shell-file-name) ; just to get a filename example

(ert-deftest a13_learn-ocaml-file-path ()
  (let* ((path example-file)
         (dir (file-name-directory path))
         (file (file-name-nondirectory path)))
    (should (string-equal (learn-ocaml-file-path (directory-file-name dir) file) path))
    (should (string-equal (learn-ocaml-file-path dir file) path))
    (should (string-equal (learn-ocaml-file-path "/dummy" path) path))))

;;; learn-ocaml-tests.el ends here
