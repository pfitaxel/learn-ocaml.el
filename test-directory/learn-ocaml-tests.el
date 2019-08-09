;; -*- lexical-binding: t; -*-

(require 'ert-async)
(require 'learn-ocaml)
;;tests for core functions
(ert-deftest-async 1_learn-ocaml-server-mangement-test (done)
  (let ((tests (lambda (callback)
		 (learn-ocaml-use-metadata
		  nil
		  "http://localhost:8080"
		  (lambda (_)
		    (learn-ocaml-give-server
		   (lambda (given-server)
		     (should (string-equal
			      "http://localhost:8080"
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
  (shell-command (concat "rm -f " learn-ocaml-temp))
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
  (shell-command "rm -f demo.ml")
  (let ((test (lambda(callback)
		(learn-ocaml-download-server-file
		 :callback (lambda (s)
			     (should (= (shell-command "cat demo.ml" 0)))
			     (shell-command "rm -f demo.ml")			     
			     (funcall callback))
		 :id "demo"))))
    (funcall test done)))

(ert-deftest-async 5_learn-ocaml-download-template-test (done)
  (shell-command "rm -f demo.ml")
  (let ((test (lambda (callback)
 		(learn-ocaml-download-template
 		 :id "demo"
 		 :callback (lambda (s)
 			     (should
 			      (=
 			       (shell-command "diff demo.ml test-directory/template_demo.ml")
 			       0))
 			     (shell-command "rm demo.ml")
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
     (shell-command "rm -fr ~/.config/learnocaml/client.json")
     (learn-ocaml-init-function
      :new-server-value "http://localhost:8080"
      :new-token-value token
      :callback (lambda (_)
		  (learn-ocaml-give-token
		   (lambda (token2)
		     (should (equal token token2))
		     (funcall done))))))))
     
(ert-deftest-async a11_learn-ocaml-on-load-test-create-token-no-config (done)
  (shell-command "rm -fr ~/.config/learnocaml/client.json")
  (learn-ocaml-init-function
      :new-server-value "http://localhost:8080"
      :nickname "test"
      :secret "test"
      :callback (lambda (_)
		  (learn-ocaml-give-token
		   (lambda (token2)
		     (funcall done))))))
  
