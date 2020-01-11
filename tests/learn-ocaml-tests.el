;; -*- lexical-binding: t; -*-

(require 'ert-async)
;(setq ert-async-timeout 2)

(require 'learn-ocaml)
(setq learn-ocaml-fail-noisely t)

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

(defun learn-ocaml-test-remove-demo-file (&optional shouldexist)
  (if shouldexist
      (shell-command (concat "rm " learn-ocaml-test-demo-file))
    (shell-command (concat "rm -f " learn-ocaml-test-demo-file))))

(defun learn-ocaml-test-remove-client-file ()
  (shell-command (concat "rm -f " learn-ocaml-test-client-file)))

(defun learn-ocaml-test-remove-temp-file ()
  (let ((file (learn-ocaml-temp-html-file)))
    (shell-command (concat "rm -f " file))))

;; Tests for core functions

(ert-deftest-async 1_learn-ocaml-server-mangement-test (done)
  (let ((tests (lambda (callback)
		 (learn-ocaml-use-metadata
		  nil
		  learn-ocaml-test-url
		  (lambda (_)
		    (learn-ocaml-give-server
		   (lambda (given-server)
		     (should (string-equal
			      learn-ocaml-test-url
			      given-server))
		     (funcall callback))))))))
    (funcall tests done)))

(ert-deftest-async 2_learn-ocaml-token-management-test (done)
  (let ((tests (lambda (callback)
		 (learn-ocaml-create-token
		  "test"
		  "test"
		  (lambda (token)
		    (learn-ocaml-use-metadata
		     token
		     nil
		     (lambda (_)
		       (learn-ocaml-give-token
			(lambda (given_token)
			  (should
			   (string-equal
			    given_token
			    token ))
		       (funcall callback))))))))))
    (funcall tests done)))


(ert-deftest-async 3_learn-ocaml-grade-test(done)
  (learn-ocaml-test-remove-temp-file)
  (let ((test (lambda(callback)
		(learn-ocaml-grade-file
		 :id "demo"
		 :file learn-ocaml-test-tograde-file
		 :callback (lambda (_)
			     (should (= (shell-command
					 (concat
					  "cat "
					  (learn-ocaml-temp-html-file)
					  " | grep \"Exercise complete\"")
					 )
					0))
                             (learn-ocaml-test-remove-temp-file)
			     (funcall callback))))))
    (funcall test done)))

(ert-deftest-async 4_learn-ocaml-download-server-file-test (done)
  (learn-ocaml-test-remove-demo-file)
  (let ((test (lambda(callback)
		(learn-ocaml-download-server-file
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
 		(learn-ocaml-download-template
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
		  (learn-ocaml-give-exercise-list
		   (lambda (json)
		     (should (equal json expected))
		     (funcall callback))))))))
  (funcall test done)))
		   
(ert-deftest-async 7_learn-ocaml-compute-questions-url-test (done)
  (learn-ocaml-give-server
   (lambda (server)
     (learn-ocaml-give-token
      (lambda (token)
	(with-temp-buffer
	  (insert-file-contents learn-ocaml-test-description-file)
	  (let* ((url (learn-ocaml-compute-questions-url server "demo" token))
		(expected (buffer-string))
		(result (shell-command-to-string (concat "curl " url )))) 
	   (should (string-match expected result))))
	  (funcall done))))))
	       

(ert-deftest-async 8_learn-ocaml-init-another-token (done)
  (learn-ocaml-create-token
   "test"
   "test"
   (lambda (token)
     (learn-ocaml-init-function
      :new-server-value nil
      :new-token-value token
      :callback (lambda (_)
		  (learn-ocaml-give-token
		   (lambda (token2)
		     (should (equal token token2))
		     (funcall done))))))))
   

(ert-deftest-async 9_learn-ocaml-init-create-token (done)
  (learn-ocaml-give-token
   (lambda (previous-token)
     (learn-ocaml-init-function
      :new-server-value nil
      :nickname "test"
      :secret "test"
      :callback (lambda (_)
		  (learn-ocaml-give-token
		   (lambda (token2)
		     (should-not (equal previous-token token2))
		     (funcall done))))))))
  
;; tests without the config file

(ert-deftest-async a10_learn-ocaml-on-load-test-another-token-no-config (done)
  (learn-ocaml-give-token
   (lambda (token)
     (learn-ocaml-test-remove-client-file)
     (learn-ocaml-init-function
      :new-server-value learn-ocaml-test-url
      :new-token-value token
      :callback (lambda (_)
		  (learn-ocaml-give-token
		   (lambda (token2)
		     (should (equal token token2))
                     ; (learn-ocaml-test-remove-client-file)
		     (funcall done))))))))
     
(ert-deftest-async a111_learn-ocaml-on-load-test-create-token-no-config (done)
  (learn-ocaml-test-remove-client-file)
  (learn-ocaml-init-function
      :new-server-value learn-ocaml-test-url
      :nickname "test"
      :secret "test"
      :callback (lambda (_)
		  (learn-ocaml-give-token
		   (lambda (token2)
                     ; (learn-ocaml-test-remove-client-file)
		     (funcall done))))))

;; misc tests

(setq example-file shell-file-name) ; just to get a filename example

(ert-deftest a12_learn-ocaml-file-path ()
  (let* ((path example-file)
         (dir (file-name-directory path))
         (file (file-name-nondirectory path)))
    (should (string-equal (learn-ocaml-file-path (directory-file-name dir) file) path))
    (should (string-equal (learn-ocaml-file-path dir file) path))
    (should (string-equal (learn-ocaml-file-path "/dummy" path) path))))
