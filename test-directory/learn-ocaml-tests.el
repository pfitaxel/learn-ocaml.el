;; -*- lexical-binding: t; -*-

(require 'ert-async)
(require 'learn-ocaml)

;;tests for core functions
(ert-deftest-async learn-ocaml-server-mangement-test (done)
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

		   
(ert-deftest-async learn-ocaml-token-management-test (done)
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


(ert-deftest-async learn-ocaml-grade-test(done) 
  (shell-command (concat "rm -f " learn-ocaml-temp))
  (let ((test (lambda(callback)
		(learn-ocaml-grade-file
		:id "demo"
		:file "test-directory/to_grade.ml"
		:callback (lambda (_)
			    (shell-command (concat "echo \"\n\">> "learn-ocaml-temp))

			    (should (= (shell-command
					(concat
					 "cat "
					 learn-ocaml-temp
					 " | grep \"Exercise complete\"")
					)
				       0))
			    (funcall callback))))))
		    (funcall test done)))

(ert-deftest-async learn-ocaml-download-server-file-test (done)
  (shell-command "rm -f demo.ml")
  (let ((test (lambda(callback)
		(learn-ocaml-download-server-file
		 :callback (lambda (s)
			     (should (= (shell-command "cat demo.ml" 0)))
			     (shell-command "rm -f demo.ml")			     
			     (funcall callback))
		 :id "demo"))))
    (funcall test done)))

(ert-deftest-async learn-ocaml-download-template-test (done)
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
		      
  

