;; -*- lexical-binding: t; -*-
(defgroup learn-ocaml nil
  "learn-ocaml  in Emacs "
  :prefix "learn-ocaml-")

(defconst learn-ocaml-version "0.0.1")

(defconst learn-ocaml-command-name "learn-ocaml-client" )

(defconst learn-ocaml-temp "~/.learnocaml-temp.html")

(defvar learn-ocaml-log-buffer (get-buffer-create "*learn-ocaml-log*"))

(defvar learn-ocaml-warning-message
  "An error occured when executing the last command, please see the log  for more information")

(defvar learn-ocaml-server "http://localhost")


(require 'cl)
(require 'cl-lib)
(require 'browse-url )
(require 'cl-macs)

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

(defun learn-ocaml-error-handler (callback proc string)
  (if (not (= (process-exit-status proc) 0))
      (message-box learn-ocaml-warning-message)
    (when callback (funcall callback string))))

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
   :buffer learn-ocaml-log-buffer
   :sentinel (apply-partially #'learn-ocaml-error-handler
			      (lambda (s) (message-box "File downloaded correctly")))))

(defun learn-ocaml-file-writter-filter (proc string)
  (write-region string nil learn-ocaml-temp t))  

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
   :sentinel (apply-partially
	      #'learn-ocaml-error-handler
	      (lambda (proc string)
		(if (string-equal string "finished\n")
		    (browse-url-firefox learn-ocaml-temp ))))))

(defun learn-ocaml-give-token (callback)
  "Gives the current token"
  (make-process
   :name "give-token"
   :command (learn-ocaml-command-constructor
	     :command "print-token"
	     )
   :stderr learn-ocaml-log-buffer
   :filter (lambda (proc string)
	     (when (= (process-exit-status proc) 0)
	       (funcall-interactively callback (replace-regexp-in-string "\n\\'" "" string))))
   :sentinel (apply-partially #'learn-ocaml-error-handler nil)))  

(defun learn-ocaml-give-server (callback)
  "Gives the current server"
  ;;(make-process
  ;; :name "give-server"
  ;; :command (learn-ocaml-command-constructor
;;	     :command "print-server"
;;	     )
  ;; :stderr learn-ocaml-log-buffer
   ;;:filter (lambda (proc string)
;;	     (when (= (process-exit-status proc) 0)
;;	       (funcall-interactively callback (replace-regexp-in-string "\n\\'" "" string))))
  ;; :sentinel (apply-partially #'learn-ocaml-error-handler nil)))
  ;; to uncomment when pr merged
  (funcall callback "http://localhost")) 

(defun learn-ocaml-use-metadata (token server callback)
  (make-process
   :name "use-metadata"
   :command (learn-ocaml-command-constructor
	     :token token
	     :server server  
	     :command "set-options" 
	     )
   :stderr learn-ocaml-log-buffer
   :sentinel (apply-partially #'learn-ocaml-error-handler callback)))

(defun learn-ocaml-show-metadata ()
  (interactive)
  (learn-ocaml-give-token
   (lambda (token)
     (learn-ocaml-give-server
      (lambda (server)
  (message-box " Current token: %s \n Current server: %s" token server))))))

(defun learn-ocaml-create-token (nickname secret callback)
  "Creates a new token"
  (make-process
   :name  "create-token"
   :command (learn-ocaml-command-constructor
	     :command "create-token"
	     :nickname nickname
	     :secret secret
	     )
   :stderr learn-ocaml-log-buffer
   :filter (apply-partially #'learn-ocaml-error-handler callback)))

(defun learn-ocaml-on-load-to-wrap (token server)
  (let ((new-server-value (if (string-equal server "")
			 (progn
			   (message-box "No server found please enter the server")
			   (read-string "Enter server: "))
			 nil))
	(new-token-value (cl-destructuring-bind (token-phrase use-found-token use-another-token )
			     (if (not (string-equal token ""))
				 `(,(concat "Token found:  " token ) ("Use found token" . 0) ("Use another token" . 1))
			       '("No token found " "Use found token" ("Use existing token" . 1)))
			 (case (x-popup-dialog
				t `(,(concat token-phrase " \n What do you want to do ? \n")
				    ,use-found-token
				    ,use-another-token
				    ("Create-new-token" . 2)))	   
			   (0 nil) 
			   (1 (read-string "Enter token: "))
			   (2 (let ((nickname
				     (read-string "What nickname you want to use for the token ? "))
				    (secret
				     (read-string "What secret do you want to associate to this token? ")))
				    (learn-ocaml-create-token nickname secret
							      (lambda (token) (learn-ocaml-use-metadata token server
													(lambda (x)
													  (message-box "Token created succesfully")
													  (learn-ocaml-show-metadata)))))
				    'creating))))))
    (when (not (eq new-token-value 'creating))
    (learn-ocaml-use-metadata new-token-value new-server-value
			      (lambda (x) (learn-ocaml-show-metadata))))))


(defun learn-ocaml-on-load-wrapped ()
  "Function to execute when loading the mode"
  (learn-ocaml-give-token
   (lambda (token)
     (learn-ocaml-give-server
      (lambda (server)
	(learn-ocaml-on-load-to-wrap token server))))))

  

