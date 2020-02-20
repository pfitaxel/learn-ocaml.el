;;; learn-ocaml.el --- Emacs frontend for learn-ocaml -*- lexical-binding: t; -*-

;; Copyright (c) 2019-2020  The learn-ocaml.el developers

;; This software is free software; you can redistribute it and/or
;; modify it under the terms of the MIT License.

;; You should have received a copy of the MIT License along with this
;; program.  If not, see <https://spdx.org/licenses/MIT>

;;; Commentary:
;;
;; learn-ocaml.el is an Emacs frontend for students using learn-ocaml.
;;

(require 'cl)
(require 'cl-lib)
(require 'browse-url)
(require 'cl-macs)
(require 'json)


;;; Code:

(defgroup learn-ocaml nil
  "learn-ocaml in Emacs "
  :prefix "learn-ocaml-")

(defvar learn-ocaml-fail-noisely nil
  "Set `learn-ocaml-fail-noisely' to non-nil for `ert'-testing purposes.")

(defconst learn-ocaml-version "0.0.1")

(defconst learn-ocaml-command-name "learn-ocaml-client")

; TODO: upon exit, propose to remove the temporary folder
(defconst learn-ocaml-temp-prefix "learn-ocaml-mode"
  "Prefix of the HTML temporary directory given to `make-temp-file'.")

(defconst learn-ocaml-temp-html-file "results.html"
  "Constant filename created in temporary folder.")

(defvar learn-ocaml-temp-dir nil)

(defvar learn-ocaml-log-buffer nil)

(defun learn-ocaml-log-buffer ()
  (unless (buffer-live-p learn-ocaml-log-buffer)
    (setq learn-ocaml-log-buffer (get-buffer-create "*learn-ocaml-log*")))
    learn-ocaml-log-buffer)

(defvar learn-ocaml-warning-message
  "An error occured when executing the last command, Do you want to open the log to have more information?")

(defvar learn-ocaml-loaded nil)

(defvar-local learn-ocaml-exercise-id nil)

;;
;; Utilitary functions
;;

(defun learn-ocaml--rstrip (str)
  (replace-regexp-in-string "\n\\'" "" str))

(defun learn-ocaml-yes-or-no (message)
  (x-popup-dialog
   t
   `(,message
     ("Yes" . t)
     ("No" . nil))))

(defun learn-ocaml-print-time-stamp ()
  (set-buffer (learn-ocaml-log-buffer))
  (goto-char (point-max))
  (insert (concat
             "\n\n\n"
             "--------------------- "
             (current-time-string)
             " ---------------------\n"
             )))

(defun learn-ocaml-file-writter-filter (file _proc string)
  (write-region string nil file t))

(defun learn-ocaml-temp-dir ()
  (let ((result (or learn-ocaml-temp-dir
                    (make-temp-file learn-ocaml-temp-prefix t))))
    (setq learn-ocaml-temp-dir result)
    result))

(defun learn-ocaml-cd (directory)
  (let ((old default-directory))
    (cd directory)
    old))

;;
;; Core functions
;;

(defun learn-ocaml-file-path (dir file)
  (if (file-name-absolute-p file)
      file
    (if (directory-name-p dir)
        (concat dir file)
      (concat (file-name-as-directory dir) file))))

; Todo: a small integration test?
(defun learn-ocaml-temp-html-file ()
  (learn-ocaml-file-path (learn-ocaml-temp-dir) learn-ocaml-temp-html-file))

(defun learn-ocaml-update-exec-path (default-dir)
  "Propose to add a directory, e.g. DEFAULT-DIR, to `exec-path'."
  (if noninteractive
      (if default-dir
          (add-to-list 'exec-path default-dir)
        (error "No directory selected (in learn-ocaml-update-exec-path)"))
    (let ((dir (read-directory-name "Add folder containing learn-ocaml-client: "
                                    (or default-dir "~/")
                                    (or default-dir "~/")
                                    t "")))
      (add-to-list 'exec-path dir))))

(cl-defun make-process-wrapper (&rest args &key command &allow-other-keys)
  "Call `make-process' after checking that the program, if the same as
`learn-ocaml-command-name', is in the `exec-path'. Otherwise query the
user to add \"opam var bin\" in `exec-path'."
  (if (and (listp command) (string-equal learn-ocaml-command-name
                                         (car command))
           (not (executable-find (car command)))) ; not found
           (if (executable-find "opam")
              (progn (message "\"%s\" not found; running \"opam var bin\"..."
                              learn-ocaml-command-name)
                     (let ((client-bin
                            (learn-ocaml--rstrip (shell-command-to-string "opam var bin"))))
                       (learn-ocaml-update-exec-path client-bin)))
             (learn-ocaml-update-exec-path nil)))
  (if (executable-find learn-ocaml-command-name)
      (apply #'make-process args)
    (if noninteractive
        (error "\"%s\" not found!" learn-ocaml-command-name)
      (message-box "\"%s\" not found.\n\n Current value of exec-path:\n%s"
                   learn-ocaml-command-name
                   (concat "(\n" (apply #'concat
                                      (map 'list (lambda (s) (concat s "\n"))
                                           exec-path)) ")"))
      (apply #'make-process-wrapper args) ; this could be a loop
      )))

(defun learn-ocaml-error-handler (buffer callback proc string)
  (let ((result (if (not buffer)
                    ""
                      (set-buffer buffer)
                      (buffer-string))))
    (when buffer (kill-buffer buffer))
    (if  (or (string-equal string "finished\n")
	     (string-match "give-token" (process-name proc))
	     (string-match "give-server" (process-name proc)))
        (funcall callback result)
      (if (learn-ocaml-yes-or-no learn-ocaml-warning-message)
	  (switch-to-buffer-other-window "*learn-ocaml-log*")
	(when learn-ocaml-fail-noisely
	  (save-excursion
	    (set-buffer (learn-ocaml-log-buffer))
	    ;; Remark: the log will contain earlier, unrelated info...
	    (let ((log (buffer-string)))
	      (error "Process errored.  Full log:\n%s" log))))))))

(cl-defun learn-ocaml-command-constructor (&key command token server local id html dont-submit param1 param2)
  (let* ((server-option (if server
                            (concat "--server=" server)
                          nil))
         (token-option (if token
                           (concat "--token=" token)
                         nil))
         (local-option (if local
                           "--local"
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
         (list (list learn-ocaml-command-name command token-option server-option id-option html-option dont-submit-option local-option param1 param2)))
    (cl-remove-if-not 'stringp list)))

(defun learn-ocaml-client-version ()
  (shell-command-to-string
   (concat (shell-quote-argument learn-ocaml-command-name) " --version")))

(cl-defun learn-ocaml-init (&key token server token nickname secret callback)
  (learn-ocaml-print-time-stamp)
  (make-process-wrapper
   :name "init"
   :command (learn-ocaml-command-constructor
             :token token
             :server server
	     :param1 nickname
	     :param2 secret
	     :command "init"
             )
   :stderr (learn-ocaml-log-buffer)
   :sentinel (apply-partially
              #'learn-ocaml-error-handler
              nil
              callback)))

(cl-defun learn-ocaml-download-server-file (&key token server id directory callback)
  "enables the user to download last version of the exercise submitted to the server
`id` should be valid"
  (learn-ocaml-print-time-stamp)
  (let ((old (learn-ocaml-cd directory)))
  (make-process-wrapper
   :name (concat "download-" id)
   :command (learn-ocaml-command-constructor
             :token token
             :server server
             :param1 id
             :command "fetch")
   :stderr (learn-ocaml-log-buffer)
   :buffer (learn-ocaml-log-buffer)     ; Todo/Erik: is it OK?
   :sentinel (apply-partially
              #'learn-ocaml-error-handler
              nil
              callback))
  (cd old)))

(cl-defun learn-ocaml-download-template (&key id token server local directory callback)
  (learn-ocaml-print-time-stamp)
  (let ((old (learn-ocaml-cd directory)))
  (make-process-wrapper
   :name (concat "template-" id)
   :command (learn-ocaml-command-constructor
             :command "template"
             :token token
             :server server
             :local local
             :param1 id
             )
   :stderr (learn-ocaml-log-buffer)
   :sentinel (apply-partially
              #'learn-ocaml-error-handler
              nil
              callback))
  (cd old)))

(cl-defun learn-ocaml-grade-file (&key id token server dont-submit file callback)
  "Grade a .ml file, optionally submitting the code and the note to the server."
  (learn-ocaml-print-time-stamp)
  (let ((html (learn-ocaml-temp-html-file)))
  (make-process-wrapper
   :name (concat "upload-" id)
   :command (learn-ocaml-command-constructor
             :token token
             :server server
             :id id
             :dont-submit dont-submit
             :param1 file
             :html t
             )
   :stderr (learn-ocaml-log-buffer)
   :filter (apply-partially
            #'learn-ocaml-file-writter-filter
            html)
   :sentinel (apply-partially
              #'learn-ocaml-error-handler
              nil
              (lambda (s)
                (funcall-interactively
                 callback
                 html))))))

(defun learn-ocaml-give-token (callback)
  "Gives the current token to the CALLBACK."
  (learn-ocaml-print-time-stamp)
  (let ((buffer (generate-new-buffer "give-token")))
    (make-process-wrapper
     :name "give-token"
     :command (learn-ocaml-command-constructor
               :command "print-token"
               )
     :stderr (learn-ocaml-log-buffer)
     :buffer buffer
     :sentinel (apply-partially
                #'learn-ocaml-error-handler
                buffer
                (lambda (s)
                  (funcall-interactively
                   callback
                   (learn-ocaml--rstrip s)))))))

(defun learn-ocaml-give-server (callback)
  "Gives the current server url to the CALLBACK."
  (learn-ocaml-print-time-stamp)
  (let ((buffer (generate-new-buffer "give-server")))
    (make-process-wrapper
     :name "give-server"
     :command (learn-ocaml-command-constructor
               :command "print-server"
               )
     :stderr (learn-ocaml-log-buffer)
     :buffer buffer
     :sentinel (apply-partially
                #'learn-ocaml-error-handler
                buffer
                (lambda (s)
                  (funcall-interactively
                   callback
                   (learn-ocaml--rstrip s)))))))

(defun learn-ocaml-use-metadata (token server callback)
  (learn-ocaml-print-time-stamp)
  (make-process-wrapper
   :name "use-metadata"
   :command (learn-ocaml-command-constructor
             :token token
             :server server
             :command "set-options"
             )
   :stderr (learn-ocaml-log-buffer)
   :sentinel (apply-partially
              #'learn-ocaml-error-handler
              nil
              callback)))

(defun learn-ocaml-create-token (nickname secret callback)
  "Create a new token for NICKNAME.
Argument SECRET may be needed by the server.
Argument CALLBACK will receive the token."
  (learn-ocaml-print-time-stamp)
  (let ((buffer (generate-new-buffer "create-token")))
    (make-process-wrapper
     :name "create-token"
     :command (learn-ocaml-command-constructor
               :command "create-token"
               :param1 nickname
               :param2 secret
               )
     :stderr (learn-ocaml-log-buffer)
     :buffer buffer
     :sentinel (apply-partially
                #'learn-ocaml-error-handler
                buffer
                (lambda (s)
                  (funcall-interactively
                   callback
                   (replace-regexp-in-string "\n\\'" "" s)))))))

(defun learn-ocaml-give-exercise-list (callback)
  "Gives to the CALLBACK a json containing the exercise list."
  (learn-ocaml-print-time-stamp)
  (let ((buffer (generate-new-buffer "exercise-list")))
    (make-process-wrapper
     :name "exercise-list"
     :command (learn-ocaml-command-constructor
	       :command "exercise-list")
     :stderr (learn-ocaml-log-buffer)
     :buffer buffer
     :sentinel (apply-partially
		#'learn-ocaml-error-handler
		buffer
		(lambda (s)
		  (funcall-interactively
		   callback (json-read-from-string s)))))))

(defun learn-ocaml-compute-questions-url (server id token)
  (concat server "/description/" id "#token=" token))

;;
;; Wrappers
;;

(defun learn-ocaml-show-questions (id)
  "Open the questions for exercise ID in the default browser."
  (interactive)
  (learn-ocaml-give-server
   (lambda (server)
     (learn-ocaml-give-token
      (lambda (token)
        ;; very important if you don't do it you risk to open eww
	(setq browse-url-browser-function 'browse-url-default-browser)
	(browse-url (learn-ocaml-compute-questions-url server id token)))))))
			    
(defun learn-ocaml-show-metadata ()
  (interactive)
  (learn-ocaml-give-token
   (lambda (token)
     (learn-ocaml-give-server
      (lambda (server)
  (message-box "Current token: %s\nCurrent server: %s" token server))))))


(cl-defun learn-ocaml-create-token-wrapper (nickname secret)
  (interactive "sWhat nickname you want to use for the token ? \nsWhat secret do you want to associate to this token? ")
  (learn-ocaml-create-token
   nickname
   secret
   (lambda (token)
     (learn-ocaml-use-metadata
      token
      nil
      (lambda (_)
        (message-box "Token created succesfully")
        (learn-ocaml-show-metadata))))))



(defun learn-ocaml-change-server()
  (interactive)
  (learn-ocaml-give-server
   (lambda (s)
     (if (learn-ocaml-yes-or-no
          (concat "The current configured server is: " s "\n Do you want to change it ?"))
         (let ((server (read-string "Enter server: ")))
           (learn-ocaml-use-metadata nil
                                     server
                                     (lambda (_)
                                       (message-box "Server changed succesfully")
                                       (learn-ocaml-show-metadata))))))))

(defun learn-ocaml-change-token ()
  (interactive)
  (learn-ocaml-give-token
   (lambda (token)
     (if (learn-ocaml-yes-or-no
          (concat "The current configured token is: "
                  token
                  "\n Do you want to change it ?"))
         (let ((token (read-string "Enter token: ")))
           (learn-ocaml-use-metadata token
                                     nil
                                     (lambda (_)
                                       (message-box "Token changed succesfully")
                                       (learn-ocaml-show-metadata))))))))

(defun learn-ocaml-download-server-file-wrapper (id &optional directory)
  (interactive `(,(let ((input (read-string(concat
                                            "Enter the id of the exercise (default "
                                            learn-ocaml-exercise-id
                                            "): "))))
                    (if (string-equal "" input)
                        learn-ocaml-exercise-id
                      input
                      ))))
  (learn-ocaml-download-server-file
   :id id
   :directory (or directory default-directory)
   :callback (lambda (_) (message-box "File(s) downloaded correctly"))))

(defun learn-ocaml-download-template-wrapper (id &optional directory)
  (interactive `(,(let ((input (read-string(concat
                                            "Enter the id of the exercise (default "
                                            learn-ocaml-exercise-id
                                            "): "))))
                    (if (string-equal "" input)
                        learn-ocaml-exercise-id
                      input
                      ))))
  (learn-ocaml-download-template
   :id id
   :directory (or directory default-directory)
   :callback (lambda (_) (message-box "Template downloaded correctly"))))

;;;###autoload
(defun learn-ocaml-grade-wrapper()
  (interactive)
  (learn-ocaml-grade-file
   :id learn-ocaml-exercise-id
   :file buffer-file-name
   :callback (lambda (html)
	       (setq browse-url-browser-function 'browse-url-default-browser)
	       (browse-url-of-file html))))
;;
;; exercise list display
;;

(require 'widget)
(define-widget 'learn-ocaml-button 'push-button ""
  :button-face 'button)

(defface learn-ocaml-group-title-face
  '((t (
	:inherit variable-pitch
	:foreground "blue1"
	:underline nil :weight bold :height 1.2)))
  "Face group titles.")

(define-widget 'learn-ocaml-group-title 'lazy ""
  :format "%{%t%}"
  :sample-face 'learn-ocaml-group-title-face)

(define-widget 'learn-ocaml-exercise-title 'lazy ""
  :format "%{%t%}"
  :sample-face 'bold)

(defun learn-ocaml-print-exercise-info (indent tuple)
  (let* ((id (elt tuple 0))
	 (exo (elt tuple 1))
	 (title (assoc-default 'title exo) )
	 (short_description (assoc-default 'short_description (elt tuple 1)))
	 (stars (assoc-default 'stars exo)))
    (widget-insert "\n")
    (widget-insert indent)
    (widget-create 'learn-ocaml-exercise-title
		   :tag title)
    (widget-insert " \n")
    (widget-insert (concat indent " "))
    (widget-insert (if short_description short_description "No description available"))
    (widget-insert " \n")
    (widget-insert (concat indent " "))
    (widget-insert (concat "Difficulty: " (number-to-string stars) "/4"
		   "    id: " id))
    (widget-insert " \n")
    (widget-insert (concat indent " "))
    (widget-create 'learn-ocaml-button
		   :notify (lambda (&rest ignore)
			     (learn-ocaml-show-questions id))
		   "Browse subject")
    (widget-insert " ")
    (widget-create 'learn-ocaml-button
		   :notify (lambda (&rest ignore)
			     (learn-ocaml-download-template-wrapper id))
		   "Get template")
    (widget-insert " ")
    (widget-create 'learn-ocaml-button
		   :notify (lambda (&rest ignore)
			     (find-file (concat id ".ml")))
		   "Open .ml")
    (widget-insert " ")
    (widget-create 'learn-ocaml-button
		   :notify (lambda (&rest ignore)
			     (learn-ocaml-download-server-file-wrapper id))
		   "Get last saved version")
    (widget-insert "\n")))

(defun learn-ocaml-print-groups (indent list)
       (let ((head (car list))
	     (queue (cdr list)))
	 (if (eq 'groups head)
	     (progn
	       (seq-do
		(lambda (group)
		  (widget-create
		   'learn-ocaml-group-title
		   :tag (concat indent
			   (cdr(car(cdr group)))
			   "\n"))
		  (learn-ocaml-print-groups (concat indent " ") (car(cdr(cdr group)))))
		queue))
	   (seq-do  (lambda (elt)
		     (learn-ocaml-print-exercise-info
		      (concat indent " ") elt))
		    queue)
	   (widget-insert "\n"))))

(defun learn-ocaml-display-exercise-list-to-wrap (json)
  "Render the exercise list from the server-provided JSON."
  (switch-to-buffer "*learn-ocaml-exercise-list*")
  (kill-all-local-variables)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)
  (learn-ocaml-print-groups "" json)
  (use-local-map widget-keymap)
  (widget-setup))

(defun learn-ocaml-display-exercise-list ()
  (interactive)
  (learn-ocaml-give-exercise-list
   (lambda (brut-json)
     (learn-ocaml-display-exercise-list-to-wrap (elt (elt brut-json 0) 1)))))

;;
;; on-load management
;;

(cl-defun learn-ocaml-init-function
    (&key new-server-value new-token-value nickname secret callback)
  (if (not new-server-value)
      ;; with config file
      (progn
	(if (not new-token-value)
	    ;; create token
	    (learn-ocaml-create-token
	     nickname
	     secret
	     (lambda (token)
	       (learn-ocaml-use-metadata
		token
		nil
		callback)))
	  ;; use token
	  (learn-ocaml-use-metadata
	   new-token-value
	   nil
	   callback)))
    ;; without config file
    (learn-ocaml-init
     :server new-server-value
     :token new-token-value
     :nickname nickname
     :callback callback)))

  
(defun learn-ocaml-on-load-to-wrap (token server callback)
  (let* ((new-server-value (if (not(string-equal server ""))
                              nil
			     (message-box "No server found please enter the server")
			     (read-string "Enter server: ")))
	 (rich-callback (lambda (_)
			  (funcall callback)
			  (learn-ocaml-show-metadata))))
    (cl-destructuring-bind (token-phrase use-found-token use-another-token)
	(if (not (string-equal token ""))
	    `(,(concat "Token found: " token) ("Use found token" . 0) ("Use another token" . 1))
	  '("No token found " "Use found token" ("Use existing token" . 1)))
      (case (x-popup-dialog
	     t `(,(concat token-phrase " \n What do you want to do ? \n")
		 ,use-found-token
		 ,use-another-token
		 ("Create-new-token" . 2)))
	(0 (funcall rich-callback nil))
	
	(1 (let ((token (read-string "Enter token: ")))
	     (learn-ocaml-init-function
	      :new-server-value new-server-value
	      :new-token-value token
	      :callback rich-callback)))
	
	(2 (let ((nickname (read-string "What nickname do you want to use for the token? "))
		 (secret (read-string "What secret do you want to associate to this token? ")))
	     (learn-ocaml-init-function
	      :new-server-value new-server-value
	      :nickname  nickname
	      :secret secret
	      :callback rich-callback)))))))
	      

(defun learn-ocaml-on-load-wrapped (callback)
  "Call `learn-ocaml-on-load-to-wrap' and CALLBACK when loading mode."
  (learn-ocaml-give-token
   (lambda (token)
     (learn-ocaml-give-server
      (lambda (server)
        (learn-ocaml-on-load-to-wrap token server callback))))))

;;
;; menu definition
;;

(defvar learn-ocaml-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-m C-l") #'learn-ocaml-display-exercise-list)
    (define-key map (kbd "C-c C-m l") #'learn-ocaml-display-exercise-list)
    (define-key map (kbd "C-c C-m C-m") #'learn-ocaml-grade-wrapper)
    (define-key map [menu-bar] nil)
    map))

(easy-menu-define learn-ocaml-mode-menu
  learn-ocaml-mode-map
  "LearnOCaml Mode Menu."
  '("LearnOCaml"
    :filter
    (lambda (list-items)
      (let ((name (buffer-file-name)))
        (if (and (<= 3 (length name))  ; nil if (buffer-file-name)=nil
                 (string-equal ".ml" (substring name -3 nil)))
            list-items                 ; keep all items for a .ml file
          (cl-remove-if (lambda (item) ; remove "Grade" otherwise
                          (and (vectorp item)
                               (string-equal "Grade" (aref item 0))))
                        list-items))))
    ["Show metadata" learn-ocaml-show-metadata]
    ["Change server" learn-ocaml-change-server]
    ["Change token" learn-ocaml-change-token]
    ["Create token" learn-ocaml-create-token-wrapper]
    "---"
    ["Show exercise list" learn-ocaml-display-exercise-list]
    ["Download template" learn-ocaml-download-template-wrapper]
    ["Download server version" learn-ocaml-download-server-file-wrapper]
    ["Grade" learn-ocaml-grade-wrapper]
    ))
;;
;; id management
;;


(defun learn-ocaml-compute-exercise-id ()
  (when buffer-file-name
    (setq-local learn-ocaml-exercise-id
        (file-name-sans-extension (file-name-base  buffer-file-name)))))

(defun learn-ocaml-update-exercise-id-view ()
  (unless learn-ocaml-exercise-id
    (learn-ocaml-compute-exercise-id))
  (define-key-after
    learn-ocaml-mode-map
    [menu-bar exercise-id]
     `(,(concat "Exercise-id: " learn-ocaml-exercise-id) .
          ,(make-sparse-keymap "Exercise-id"))
     )
  (define-key
    learn-ocaml-mode-map
    [menu-bar exercise-id reset]
    '("Reset id" . learn-ocaml-exercise-id-initializer))
  (define-key
    learn-ocaml-mode-map
    [menu-bar exercise-id change]
    '("Change id" . learn-ocaml-change-exercise-id))
  (force-mode-line-update)) ; a random instruction is needed to update menu bar

(defun learn-ocaml-exercise-id-initializer ()
  (interactive)
  (setq-local learn-ocaml-exercise-id nil)
  (learn-ocaml-update-exercise-id-view))

(defun learn-ocaml-change-exercise-id (new-id)
  (interactive "sEnter new id : ")
  (setq-local learn-ocaml-exercise-id new-id)
  (learn-ocaml-update-exercise-id-view))

(add-hook 'window-configuration-change-hook #'learn-ocaml-update-exercise-id-view)

;;
;; definition of the mode
;;

;;;###autoload
(define-minor-mode learn-ocaml-mode
  "learn-ocaml in Emacs"
  :lighter " LearnOCaml"
  :keymap learn-ocaml-mode-map
  (if (bound-and-true-p learn-ocaml-mode)
      (progn
	(learn-ocaml-update-exercise-id-view)
	(easy-menu-add learn-ocaml-mode-menu)
	(unless learn-ocaml-loaded
	  (add-hook 'caml-mode-hook #'learn-ocaml-mode)
	  (add-hook 'tuareg-mode-hook #'learn-ocaml-mode)
	  (learn-ocaml-on-load-wrapped
	   (lambda ()
	     (when (learn-ocaml-yes-or-no
		    "Do you want to open the list of exercises available on the server ?")
               (let ((dir
                      (read-directory-name "Choose your working directory: "
                                           default-directory
                                           default-directory
                                           nil "")))
                     (make-directory dir t)
                     (setq default-directory dir))
	       (learn-ocaml-display-exercise-list))))
	  (setq learn-ocaml-loaded t)))
    (setq learn-ocaml-loaded nil)
    (remove-hook 'caml-mode-hook #'learn-ocaml-mode)
    (remove-hook 'tuareg-mode-hook #'learn-ocaml-mode)))

(provide 'learn-ocaml)

;;; learn-ocaml.el ends here
