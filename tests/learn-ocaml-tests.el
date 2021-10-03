;;; learn-ocaml-tests.el --- unit tests -*- lexical-binding: t; -*-

;; Copyright (c) 2019-2021  The learn-ocaml.el developers

;; This software is free software; you can redistribute it and/or
;; modify it under the terms of the MIT License.

;; You should have received a copy of the MIT License along with this
;; program.  If not, see <https://spdx.org/licenses/MIT>

;;; Commentary:
;;
;; This file sets the test infrastructure: fixtures, helper functions.
;;

(require 'learn-ocaml)

;;; Code:

(setq learn-ocaml-fail-noisely t)

(require 'ert-async)
;(setq ert-async-timeout 2)

(setq learn-ocaml-test-url "http://localhost:8080")

;; REMARK: some tests also rely on the "curl" binary

(defvar learn-ocaml-test-use-passwd nil
  "Should be non-nil if USE_PASSWD=true; used by `learn-ocaml-test-run-with'.")

(defun learn-ocaml-test-use-passwd-auto ()
  "Sets `learn-ocaml-test-use-passwd' automatically."
  (if (not (learn-ocaml-compat learn-ocaml-feature-passwd-compat
                               (version-to-list
                                (learn-ocaml-client-server-min-version learn-ocaml-test-url))))
      (setq learn-ocaml-test-use-passwd nil)
    (progn
      ;; (learn-ocaml-init-server-cmd
      ;; :server learn-ocaml-test-url
      ;; :callback (lambda (_) ...))
      ;; COMMENTED-OUT: needs a synchronous version
      (let ((init-server
             (learn-ocaml-command-to-string-await-cmd
              (list "init-server" "-s" learn-ocaml-test-url))))
        (if (car init-server)
            (let ((json (learn-ocaml-client-config-cmd)))
              (setq learn-ocaml-test-use-passwd
                    (string-equal
                     (cdr (assoc 'use_passwd (json-read-from-string json)))
                     "true"))
              learn-ocaml-test-use-passwd)
          (error "learn-ocaml-test-use-passwd-auto: init-server failed with [%s]." (cdr init-server)))))))

(defvar learn-ocaml-test-user-num 0
  "Numerical id of the last-created user by `learn-ocaml-test-user-email'.
See also `learn-ocaml-client-sign-up-cmd'.")

(defun learn-ocaml-test-user-email ()
  "Generate a new user email"
  (setq learn-ocaml-test-user-num (1+ learn-ocaml-test-user-num))
  (let ;; ((msecs (caddr (current-time))))
      ((rand ; for uniqueness, if emacs is restarted during the CI run
        (random 65536)))
  (format "test%d-%d@example.com" learn-ocaml-test-user-num rand)))

(defun learn-ocaml-test-user-pass ()
  "Return a dummy password for creating test accounts."
  "OCaml123_")

(defun learn-ocaml-test-secret ()
  "Return the learn-ocaml secret for creating accounts."
  "ServerPass")

;;; NOTE: This symbol list gather tests specific to 'use_passwd: true'
;;; (setq learn-ocaml-test-use-passwd-list
;;;       '(a12_learn-ocaml-test-sign-up  2_learn-ocaml-token-management-test))
;;;
;;; (setq learn-ocaml-test-skip-use-passwd
;;;       `(not (member ,@learn-ocaml-test-use-passwd-list)))

;; WARNING: several tests delete the ./demo.ml and client.json files:
(setq learn-ocaml-test-client-file "~/.config/learnocaml/client.json")

(setq learn-ocaml-test-tograde-file (expand-file-name "../to_grade.ml"))
(setq learn-ocaml-test-template-file (expand-file-name "../template_demo.ml"))
(setq learn-ocaml-test-json-file (expand-file-name "../exercise_list.json"))
(setq learn-ocaml-test-description-file (expand-file-name "../expected_description.html"))

;; This fixture is needed because of Travis CI's permission mismatch:
;; bind-mount:'uid=2000(travis)' vs. current-user:uid=1000(learn-ocaml)'.
;; The function `learn-ocaml-temp-dir' auto-creates a temp directory.
(setq learn-ocaml-fixture-directory (learn-ocaml-temp-dir))
(setq learn-ocaml-test-demo-file
      (learn-ocaml-file-path learn-ocaml-fixture-directory "demo.ml"))

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

(defun learn-ocaml-test-client-expected-path ()
  "Return ../../../learn-ocaml/_opam/bin
Assume this function is run from a subdirectory/runtests.el"
  (let ((curdir (file-name-directory
                 (or (buffer-file-name)
                     (file-name-as-directory command-line-default-directory))))
        (up (lambda (dir) (file-name-directory (directory-file-name dir)))))
    (concat (funcall up (funcall up (funcall up curdir)))
            "learn-ocaml/_opam/bin")))

(defvar learn-ocaml-test-dir nil
  "Path of the .../learn-ocaml.el/tests/00x-name/ directory.
Useful if the ERT tests are run interactively, because in this case
(buffer-file-name) returns nil.")

(defun learn-ocaml-test-dir ()
  "Set the `learn-ocaml-test-dir' variable.
Assume this function is run from a subdirectory/runtests.el"
  (let ((curdir (file-name-directory
                 (or (buffer-file-name)
                     (file-name-as-directory command-line-default-directory)))))
    (setq learn-ocaml-test-dir curdir))
    learn-ocaml-test-dir)

(defun learn-ocaml-test-get-teacher-token ()
  "Get ../../teacher.txt
Assume this function is run from a subdirectory/runtests.el"
  (let* ((curdir (file-name-directory
                  (or learn-ocaml-test-dir
                      (buffer-file-name)
                      (file-name-as-directory command-line-default-directory))))
         (up (lambda (dir) (file-name-directory (directory-file-name dir))))
         (filename (concat (funcall up (funcall up curdir))
                           "teacher.txt")))
    (with-temp-buffer
      (insert-file-contents-literally filename)
      (buffer-string))))

(defun learn-ocaml-test-get-last-confirm ()
  "Get last ../../confirm.txt
Assume this function is run from a subdirectory/runtests.el"
  (let* ((curdir (file-name-directory
                  (or learn-ocaml-test-dir
                      (buffer-file-name)
                      (file-name-as-directory command-line-default-directory))))
         (up (lambda (dir) (file-name-directory (directory-file-name dir))))
         (filename (concat (funcall up (funcall up curdir))
                           "confirm.txt")))
    (sleep-for 0.2) ;; hacky
    (with-temp-buffer
      (insert-file-contents-literally filename)
      (goto-char (point-max))
      (delete-blank-lines) ;; Remove potential trailing whitespace
      (when (and
             (= (line-beginning-position) (line-end-position))
             (> (point) (point-min)))
             (left-char 1))
      (let ((bol (line-beginning-position)) (eol (line-end-position)))
        (if (= bol eol)
            (error "learn-ocaml-test-get-last-confirm: No confirmation URL available")
          (buffer-substring-no-properties bol eol))))))

;;; Two macros that leverage `ert-deftest-async' with `mapcar' (DRY principle)

(defmacro ert-deftest-async-map-symb (name symb-list var callbacks &rest body)
  "Call (`ert-deftest-async' NAME CALLBACKS BODY) foreach VAR in SYMB-LIST.
Alter NAME accordingly with a suffix from SYMB-LIST.
Bind VAR before calling BODY."
  (declare (indent 0))
  `(progn
     ,@(mapcar ;; better than (dolist)
        (lambda (symb)
          (let ((name-symb (intern (concat (symbol-name name) "_" (symbol-name symb)))))
            `(ert-deftest-async ,name-symb ,callbacks
               (let ((,var ',symb))
                 ,@body))))
        symb-list)))

(defmacro ert-deftest-async-map (name val-list var callbacks &rest body)
  "Call (`ert-deftest-async' NAME CALLBACKS BODY) foreach VAR in VAL-LIST.
Alter NAME accordingly with a suffix from VAL-LIST.
Bind VAR before calling BODY.
If VAL-LIST contains symbols, do not quote them."
  (declare (indent 0))
  (let ((idx-list (number-sequence 0 (1- (length val-list)))))
    `(progn
       ,@(mapcar ;; better than (dotimes)
          (lambda (idx)
            (let ((val (nth idx val-list)) ;; or (elt val-list idx)
                  (name-idx (intern (concat (symbol-name name) "_"
                                            (int-to-string idx)))))
              `(ert-deftest-async ,name-idx ,callbacks
                 (let ((,var ',val))
                   ,@body))))
          idx-list))))

;;; Examples:
;;
;; (ert-deftest-async-map-symb
;;     foobar (action1 action2) action (done1 done2)
;;     (funcall done1)
;;     (message (symbol-name action))
;;     (funcall done2))
;;
;; (ert-deftest-async-map
;;     foobaz ((action1 . "Hello") (action2 . "World")) pair (done1 done2)
;;     (funcall done1)
;;     (message "%s: %s!" (symbol-name (car pair)) (cdr pair))
;;     (funcall done2))

(cl-defun learn-ocaml-test-run-with
    (&key before-action body)
  "Fixture to setup a login environment for tests.
BEFORE-ACTION should be nil, 'login-teacher or 'signup.
The caller must run (learn-ocaml-test-remove-client-file) manually afterwards."

  (learn-ocaml-test-remove-client-file) ;; pre-teardown

  ;; (let ((teardown
  ;;        (if after-remove-cookie
  ;;            (lambda () (learn-ocaml-test-remove-client-file)))
  ;;        (lambda () (message "No need for cookie file removal"))))
  ;; COMMENTED-OUT as maybe this fixture could incorporate a teardown

  (cond
   ((and before-action (not (member before-action '(login-teacher signup))))
    (error "Unexpectedly value for before-action: [%s]"
           (pp-to-string before-action)))

   ((not before-action)
    (funcall body))

   ((equal before-action 'login-teacher)
    (learn-ocaml-init-cmd ;; OK even if USE_PASSWD=true
     :server learn-ocaml-test-url
     :token (learn-ocaml-test-get-teacher-token)
     :nickname "Teacher"
     :callback (lambda (_) (funcall body))))
   ;; Note: this form completes immediately *but* the async test runs in the bg.

   ((equal before-action 'signup)
    (if (not learn-ocaml-test-use-passwd)
        (learn-ocaml-init-cmd
         :server learn-ocaml-test-url
         :nickname "Student"
         :secret (learn-ocaml-test-secret)
         :callback (lambda (_) (funcall body)))
      (let ((email (learn-ocaml-test-user-email))
            (pass (learn-ocaml-test-user-pass)))
        (learn-ocaml-client-sign-up-cmd
         :server learn-ocaml-test-url
         :login email
         :password pass
         :nickname (format "StudentWithEmail(%s)" email)
         :secret (learn-ocaml-test-secret)
         :callback-err
         (lambda (output) (error "learn-ocaml-test-run-with: learn-ocaml-client-sign-up-cmd: failed with [%s]." output))
         :callback-ok
         (lambda (_)
           (let ((url (learn-ocaml-test-get-last-confirm)))
             (shell-command
              (concat "curl -fsS " (shell-quote-argument url)
                      " ; sleep 0.2s")))
           (learn-ocaml-client-sign-in-cmd
            :server learn-ocaml-test-url
            :login email
            :password pass
            :callback-err
            (lambda (output) (error "learn-ocaml-test-run-with: learn-ocaml-client-sign-in-cmd: failed with [%s]." output))
            :callback-ok
            (lambda (_) (funcall body))))))))))

;;; learn-ocaml-tests.el ends here
