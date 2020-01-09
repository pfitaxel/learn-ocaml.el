;; -*- lexical-binding: t; -*-

(require 'ert-async)
;(setq ert-async-timeout 2)

(require 'learn-ocaml)
(setq learn-ocaml-fail-noisely t)

;; WARNING: several tests delete the ./demo.ml and client.json files:
(setq learn-ocaml-test-client-file "~/.config/learnocaml/client.json")
(setq learn-ocaml-test-demo-file "demo.ml")

;; REMARK: unless otherwise noted, the tests assume that we have previously run
;; $ learn-ocaml-client init --server=http://localhost:8080 test test
(setq learn-ocaml-test-url "http://localhost:8080")

(defun learn-ocaml-test-remove-demo-file ()
  (shell-command (concat "rm -f " learn-ocaml-test-demo-file)))

(defun learn-ocaml-test-remove-client-file ()
  (shell-command (concat "rm -f " learn-ocaml-test-client-file)))

(defun learn-ocaml-test-remove-temp-file ()
  (shell-command (concat "rm -f " learn-ocaml-temp)))

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
		 :file "test-directory/to_grade.ml"
		 :callback (lambda (_)
			     (should (= (shell-command
					 (concat
					  "cat "
					  learn-ocaml-temp
					  " | grep \"Exercise complete\"")
					 )
					0))
			     (funcall callback))))))
    (funcall test done)))

(ert-deftest-async 4_learn-ocaml-download-server-file-test (done)
  (learn-ocaml-test-remove-demo-file)
  (let ((test (lambda(callback)
		(learn-ocaml-download-server-file
		 :callback (lambda (s)
			     (should (= 0 (shell-command
					   (concat "cat " learn-ocaml-test-demo-file))))
			     (learn-ocaml-test-remove-demo-file)
			     (funcall callback))
		 :id "demo"))))
    (funcall test done)))

(ert-deftest-async 5_learn-ocaml-download-template-test (done)
  (learn-ocaml-test-remove-demo-file)
  (let ((test (lambda (callback)
 		(learn-ocaml-download-template
 		 :id "demo"
 		 :callback (lambda (s)
 			     (should
 			      (= 0 (shell-command
				    (concat "diff "
					    learn-ocaml-test-demo-file
					    " test-directory/template_demo.ml"))))
			     (learn-ocaml-test-remove-demo-file) ; without "-f" ?
 			     (funcall callback))))))
    (funcall test done)))

  
(ert-deftest-async 6_learn-ocaml-give-exercise-list-test (done)
  (let ((test (lambda (callback)
		(with-temp-buffer
		  (insert-file-contents "test-directory/exercise_list.json")
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
	  (insert-file-contents "test-directory/expected_description.html")
	  (let* ((url (learn-ocaml-compute-questions-url server "demo" token))
		(expected (buffer-string))
		(result (shell-command-to-string (concat "curl " url )))) 
	   (should-not (equal nil (string-match expected result)))))
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
		     (funcall done))))))))
     
(ert-deftest-async a11_learn-ocaml-on-load-test-create-token-no-config (done)
  (learn-ocaml-test-remove-client-file)
  (learn-ocaml-init-function
      :new-server-value learn-ocaml-test-url
      :nickname "test"
      :secret "test"
      :callback (lambda (_)
		  (learn-ocaml-give-token
		   (lambda (token2)
		     (funcall done))))))
