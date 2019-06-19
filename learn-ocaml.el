;; -*- lexical-binding: t; -*-
(defgroup learn-ocaml nil
  "learn-ocaml  in Emacs "
  :prefix "learn-ocaml-")

(defconst learn-ocaml-version "0.0.1")

(defconst learn-ocaml-command-name "learn-ocaml-client" )

(defconst learn-ocaml-temp "~/.learnocaml-temp.html")

(defvar learn-ocaml-log-buffer (get-buffer-create "*learn-ocaml-log*"))

(defvar learn-ocaml-warning-message
  "An error occured when executing the last command, Do you want to open the log to have more information?")

(defvar learn-ocaml-server "http://localhost")

(defvar-local learn-ocaml-exercise-id nil)

(require 'cl)
(require 'cl-lib)
(require 'browse-url )
(require 'cl-macs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;core functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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


(defun learn-ocaml-yes-or-no (message)
  (x-popup-dialog
   t
   `(,message
     ("Yes" . t)
     ("No" . nil))))
  
(defun learn-ocaml-error-handler (buffer callback proc string)
  (let ((result (if buffer
		    (progn
		      (set-buffer buffer)
		      (buffer-string))
		  "")))
    (when buffer (kill-buffer buffer))
    (if (not (string-equal string "finished\n"))
	(progn
	  (when (learn-ocaml-yes-or-no learn-ocaml-warning-message)
	    (switch-to-buffer-other-window "*learn-ocaml-log*")))
      (funcall callback result))))

;;todo add more verbosity to this function , return value 0 if
;;there was not a file to download
(cl-defun learn-ocaml-download-server-file (&key token server id)
  "enables the user to download last version of the exercise submitted to the server
`id` should be valid"  
  (make-process
   :name (concat "download-" id)
   :command (learn-ocaml-command-constructor
	     :token token
	     :server server
	     ;;:id id rework this once th e pr is merged
	     :command "fetch")
   :stderr learn-ocaml-log-buffer
   :buffer learn-ocaml-log-buffer
   :sentinel (apply-partially #'learn-ocaml-error-handler 
			      nil
			      (lambda (s) (message-box "File(s) downloaded correctly")))))

(defun learn-ocaml-file-writter-filter (proc string)
  (write-region string nil learn-ocaml-temp t))  

(cl-defun learn-ocaml-grade-file (&key id token server dont-submit file)
  "Grade a .ml file, optionally submitting the code and the note to the server."
  (write-region "" nil learn-ocaml-temp )
  (make-process
   :name (concat "upload-" id)
   :command (learn-ocaml-command-constructor
	     :token token 
	     :server server
	     :id id
	     :dont-submit dont-submit
	     :file file
	     :html t
	     )
   :stderr learn-ocaml-log-buffer
   :filter #'learn-ocaml-file-writter-filter
   :sentinel (apply-partially
	      #'learn-ocaml-error-handler
	      nil
	      (lambda (string)	
		    (browse-url-firefox learn-ocaml-temp )))))

(defun learn-ocaml-give-token (callback)
  "Gives the current token"
 (let ((buffer (generate-new-buffer "give-token")))
    (make-process
     :name "give-token"
     :command (learn-ocaml-command-constructor
	       :command "print-token"
	       )
     :stderr learn-ocaml-log-buffer
     :buffer buffer
     :sentinel (apply-partially #'learn-ocaml-error-handler 
				buffer
				(lambda (s) (funcall-interactively callback (replace-regexp-in-string "\n\\'" "" s))))))) 
 
(defun learn-ocaml-give-server (callback)
  "Gives the current server"
  (let ((buffer (generate-new-buffer "give-server")))
    (make-process
     :name "give-server"
     :command (learn-ocaml-command-constructor
	       :command "print-server"
	       )
     :stderr learn-ocaml-log-buffer
     :buffer buffer
     :sentinel (apply-partially #'learn-ocaml-error-handler
				buffer
				(lambda (s) (funcall-interactively callback (replace-regexp-in-string "\n\\'" "" s))))))) 

(defun learn-ocaml-use-metadata (token server callback)
  (make-process
   :name "use-metadata"
   :command (learn-ocaml-command-constructor
	     :token token
	     :server server  
	     :command "set-options" 
	     )
   :stderr learn-ocaml-log-buffer
   :sentinel (apply-partially #'learn-ocaml-error-handler nil callback)))

(defun learn-ocaml-create-token (nickname secret callback)
  "Creates a new token"
  (let ((buffer (generate-new-buffer "create-token")))
    (make-process
     :name "create-token"
     :command (learn-ocaml-command-constructor
	       :command "create-token"
	       :nickname nickname
	       :secret secret
	       )
     :stderr learn-ocaml-log-buffer
     :buffer buffer
     :sentinel (apply-partially #'learn-ocaml-error-handler
				buffer
				(lambda (s) (funcall-interactively callback (replace-regexp-in-string "\n\\'" "" s))))))) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;wrappers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun learn-ocaml-show-metadata ()
  (interactive)
  (learn-ocaml-give-token
   (lambda (token)
     (learn-ocaml-give-server
      (lambda (server)
  (message-box "Current token: %s\nCurrent server: %s" token server))))))


(defun learn-ocaml-create-token-wrapper (nickname secret)
  (interactive "sWhat nickname you want to use for the token ? \nsWhat secret do you want to associate to this token? " )
  (learn-ocaml-create-token nickname secret
			    (lambda (token) (learn-ocaml-use-metadata token nil
								      (lambda (x)
									(message-box "Token created succesfully")
									  (learn-ocaml-show-metadata))))))


(defun learn-ocaml-change-server()
  (interactive)
  (learn-ocaml-give-server (lambda (s)
			     (if (learn-ocaml-yes-or-no
				  (concat "The current configured server is: " s "\n Do you want to change it ?" ))
				 (let ((server (read-string "Enter server: "))) 
				   (learn-ocaml-use-metadata nil
							     server
							     (lambda (_)
							       (message-box "Server changed succesfully")
							       (learn-ocaml-show-metadata))))))))

(defun learn-ocaml-change-token ()
  (interactive)
  (learn-ocaml-give-token (lambda (token)
			    (if (learn-ocaml-yes-or-no
				 (concat "The current configured token is: " token "\n Do you want to change it ?" ))
				(let ((token (read-string "Enter token: "))) 
				  (learn-ocaml-use-metadata token
							    nil
							    (lambda (_)
							      (message-box "Token changed succesfully")
							      (learn-ocaml-show-metadata))))))))

(defun learn-ocaml-download-server-file-wrapper (id)
  (interactive `(,(let ((input (read-string(concat
					    "Enter the id of the exercise [default "
					    learn-ocaml-exercise-id
					    " ]: "))))
		    (if (string-equal "" input)
			learn-ocaml-exercise-id
		      input
		      ))))
  (learn-ocaml-download-server-file
   :id id))

(defun learn-ocaml-grade-wrapper()
  (interactive)
  (let ((dont-submit  (not (learn-ocaml-yes-or-no
		       "Do you want to submit the result to the server? ")))
	(file buffer-file-name))
    (learn-ocaml-grade-file
     :id learn-ocaml-exercise-id
     :file file
     :dont-submit dont-submit
     )))
			 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			 

;;;;;;;;;;;;;;;;;;;on load management ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
			   (2 (call-interactively #'learn-ocaml-create-token-wrapper)
			      'creating)))))
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



;;;;;;;;; menu definition ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar learn-ocaml-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-as" #'learn-ocaml-show-metadata)
    (define-key map [menu-bar] nil )
    map))

(easy-menu-define learn-ocaml-mode-menu
  learn-ocaml-mode-map
  "Learnocaml Mode Menu."
  '("Learnocaml"
    ["Show metadata" learn-ocaml-show-metadata]
    ["Change server" learn-ocaml-change-server]
    ["Change token" learn-ocaml-change-token]
    ["Create token" learn-ocaml-create-token-wrapper]
    ["Grade" learn-ocaml-grade-wrapper]
    ["Download server version" learn-ocaml-download-server-file-wrapper]
    ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;id management ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun learn-ocaml-update-exercise-id-view ()
  (define-key-after
    learn-ocaml-mode-map
    [menu-bar exercise-id]
     `(,(concat "exercise-id: " learn-ocaml-exercise-id) .
	  ,(make-sparse-keymap "Exercise-id"))
     )
  (define-key
    learn-ocaml-mode-map
    [menu-bar exercise-id reset]
    '("Reset id" . learn-ocaml-exercise-id-initializer))
  (define-key 
    learn-ocaml-mode-map
    [menu-bar exercise-id change]
    '("Change id" . learn-ocaml-change-exercise-id ))
  (force-mode-line-update)) ;; a random instruction is needed to update menu bar

(defun learn-ocaml-exercise-id-initializer()
  (interactive)
  (setq-local learn-ocaml-exercise-id
	(file-name-sans-extension(file-name-base  buffer-file-name)))
  (learn-ocaml-update-exercise-id-view))

(defun learn-ocaml-change-exercise-id (new-id)
  (interactive "sEnter new id : ")
  (setq-local learn-ocaml-exercise-id new-id)
  (learn-ocaml-update-exercise-id-view))

(add-hook 'window-configuration-change-hook #'learn-ocaml-update-exercise-id-view)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;definition of the mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(define-minor-mode learn-ocaml-mode
  "learn-ocaml in Emacs"
  :lighter " Learnocaml"
  :keymap learn-ocaml-mode-map
  (if (bound-and-true-p learn-ocaml-mode)
      (progn
	(learn-ocaml-on-load-wrapped)
	(easy-menu-add learn-ocaml-mode-menu)
	(learn-ocaml-exercise-id-initializer))))

(provide 'learn-ocaml)
