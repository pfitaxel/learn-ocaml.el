;; -*- lexical-binding: t; -*-
(defgroup learn-ocaml nil
  "learn-ocaml  in Emacs "
  :prefix "learn-ocaml-")

(defconst learn-ocaml-version "0.0.1")

(defconst learn-ocaml-command-name "learn-ocaml-client" )

(defconst learn-ocaml-temp "~/.learnocaml-temp.html")

(defvar learn-ocaml-log-buffer (get-buffer-create "*learn-ocaml-log*"))

(defvar learn-ocaml-server "http://localhost")

(require 'cl)
(require 'cl-lib)
(require 'browse-url )

(cl-defun learn-ocaml-command-constructor (&key command token server id html dont-submit file nickname secret )
  (let* ((server-option (if server
			    (concat "--server=" server)
			  nil))
	 (token-option (if token
			   (concat "--token=" token)
			 nil))
	 (id-option (if id
			(concat "--id=" id)
		      nil))
	 (html-option (if html
			  "--html"
			nil))
	 (dont-submit-option (if dont-submit
				 "-n"
			       nil))
	 
	 (list (list learn-ocaml-command-name command token-option server-option id-option html-option dont-submit-option file nickname secret)))
    (cl-remove-if-not 'stringp list)))

(cl-defun learn-ocaml-download-server-file (&key token server id)
  "enables the user to download last version of the exercise submitted to the server
`id` should be valid"
  (make-process
   :name (concat "download-" id)
   :command (learn-ocaml-command-constructor
	     :token token
	     :server server
	     :id id
	     :command "fetch")
   :stderr learn-ocaml-log-buffer
   :buffer learn-ocaml-log-buffer ))

(defun learn-ocaml-file-writter-filter (proc string)
  (write-region string nil learn-ocaml-temp t)
  )  


(cl-defun learn-ocaml-grade-file (&key id token server dont-submit file)
  "Grade a .ml file, optionally submitting the code and the note to the server."
  (interactive)
  (write-region "" nil learn-ocaml-temp )
  (make-process
   :name (concat "upload-" id)
   :command (learn-ocaml-command-constructor
	     :token token
	     :server server
	     :id id
	     :dont-submit dont-submit
	     :file file
	     :html
	     )
   :stderr learn-ocaml-log-buffer
   :filter #'learn-ocaml-file-writter-filter
   :sentinel (lambda (proc string)
	       ""
	       (interactive)
	       (if (string-equal string "finished\n")
		   (browse-url-firefox learn-ocaml-temp )))))


(defun learn-ocaml-give-token (callback)
  "Gives the current token"
  (make-process
   :name "give-token"
   :command (learn-ocaml-command-constructor
	     :command "print-token"
	     )
   :stderr learn-ocaml-log-buffer
   :filter (lambda (proc string) (interactive) (funcall-interactively callback string ))))  

(defun learn-ocaml-give-server (callback)
  "Gives the current server"
  (make-process
   :name "give-server"
   :command (learn-ocaml-command-constructor
	     :command "print-server"
	     )
   :stderr learn-ocaml-log-buffer
   :filter (lambda (proc string) (interactive) (funcall-interactively callback string ))))  



(defun learn-ocaml-use-metadata (token server )
  (interactive "sEnter your token please \nsDefault Enter the server :http://localhost  : \n")
    (make-process
     :name "use-metadata"
     :command (learn-ocaml-command-constructor
	       :token token
	       :server server  
	       :command "set-options" 
	       )
     :stderr learn-ocaml-log-buffer
     ))

(defun learn-ocaml-create-and-use-new-token (nickname secret)
  "Creates a new token"
  (interactive "sWhat nickname you want to use for the token ? \nsWhat secret do you want to associate to this token? ")
  (make-process
   :name  "create-token"
   :command (learn-ocaml-command-constructor
	     :command "create-token"
	     :nickname nickname
	     :secret secret
	     :server learn-ocaml-server  ;;temporary fix to remove when issue 271 is solved
	     )
   :stderr learn-ocaml-log-buffer
   :filter (lambda (proc string) (interactive) 
	     (message-box "Token was created for nickname %s with secret %s : %s" nickname secret string)
	     (message-box "Using this new token now")
	     (funcall-interactively #'learn-ocaml-use-metadata string nil)
	     )))

