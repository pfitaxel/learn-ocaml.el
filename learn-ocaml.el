;;; learn-ocaml.el --- Emacs frontend for learn-ocaml -*- lexical-binding: t; -*-

;;; Commentary:
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

(defconst learn-ocaml-version "0.0.1")

(defconst learn-ocaml-command-name "learn-ocaml-client")

(defconst learn-ocaml-temp "~/.learnocaml-temp.html")

(defvar learn-ocaml-log-buffer (get-buffer-create "*learn-ocaml-log*"))

(defvar learn-ocaml-warning-message
  "An error occured when executing the last command, Do you want to open the log to have more information?")

(defvar learn-ocaml-server "http://localhost")

(defvar learn-ocaml-loaded nil)

(defvar-local learn-ocaml-exercise-id nil)

;;
;; Utilitary functions
;;

(defun learn-ocaml-yes-or-no (message)
  (x-popup-dialog
   t
   `(,message
     ("Yes" . t)
     ("No" . nil))))

(defun learn-ocaml-print-time-stamp ()
  (set-buffer learn-ocaml-log-buffer)
  (goto-char (point-max))
  (insert (concat
             "\n\n\n"
             "-----------------------------------------"
             (current-time-string)
             "----------------------------------------\n"
             )))

(defun learn-ocaml-file-writter-filter (_proc string)
  (write-region string nil learn-ocaml-temp t))



;;
;; Core functions
;;

(defun learn-ocaml-error-handler (buffer callback _proc string)

  (let ((result (if (not buffer)
                    ""
                      (set-buffer buffer)
                      (buffer-string))))
    (when buffer (kill-buffer buffer))
    (if  (string-equal string "finished\n")
        (funcall callback result)
      (when (learn-ocaml-yes-or-no learn-ocaml-warning-message)
        (switch-to-buffer-other-window "*learn-ocaml-log*")))))



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

(cl-defun learn-ocaml-download-server-file (&key token server id callback)
  "enables the user to download last version of the exercise submitted to the server
`id` should be valid"
  (learn-ocaml-print-time-stamp)
  (make-process
   :name (concat "download-" id)
   :command (learn-ocaml-command-constructor
             :token token
             :server server
             :param1 id
             :command "fetch")
   :stderr learn-ocaml-log-buffer
   :buffer learn-ocaml-log-buffer
   :sentinel (apply-partially
              #'learn-ocaml-error-handler
              nil
              callback)))

(cl-defun learn-ocaml-download-template (&key id token server local callback)
  (learn-ocaml-print-time-stamp)
  (make-process
   :name (concat "template-" id)
   :command (learn-ocaml-command-constructor
             :command "template"
             :token token
             :server server
             :local local
             :param1 id
             )
   :stderr learn-ocaml-log-buffer
   :sentinel (apply-partially
              #'learn-ocaml-error-handler
              nil
              callback)))

(cl-defun learn-ocaml-grade-file (&key id token server dont-submit file callback)
  "Grade a .ml file, optionally submitting the code and the note to the server."
  (learn-ocaml-print-time-stamp)
  (write-region "" nil learn-ocaml-temp)
  (make-process
   :name (concat "upload-" id)
   :command (learn-ocaml-command-constructor
             :token token
             :server server
             :id id
             :dont-submit dont-submit
             :param1 file
             :html t
             )
   :stderr learn-ocaml-log-buffer
   :filter #'learn-ocaml-file-writter-filter
   :sentinel (apply-partially
              #'learn-ocaml-error-handler
              nil
              callback)))

(defun learn-ocaml-give-token (callback)
  "Gives the current token."
  (learn-ocaml-print-time-stamp)
  (let ((buffer (generate-new-buffer "give-token")))
    (make-process
     :name "give-token"
     :command (learn-ocaml-command-constructor
               :command "print-token"
               )
     :stderr learn-ocaml-log-buffer
     :buffer buffer
     :sentinel (apply-partially
                #'learn-ocaml-error-handler
                buffer
                (lambda (s)
                  (funcall-interactively
                   callback
                   (replace-regexp-in-string "\n\\'" "" s)))))))

(defun learn-ocaml-give-server (callback)
  "Gives the current server."
  (learn-ocaml-print-time-stamp)
  (let ((buffer (generate-new-buffer "give-server")))
    (make-process
     :name "give-server"
     :command (learn-ocaml-command-constructor
               :command "print-server"
               )
     :stderr learn-ocaml-log-buffer
     :buffer buffer
     :sentinel (apply-partially
                #'learn-ocaml-error-handler
                buffer
                (lambda (s)
                  (funcall-interactively
                   callback
                   (replace-regexp-in-string "\n\\'" "" s)))))))

(defun learn-ocaml-use-metadata (token server callback)
  (learn-ocaml-print-time-stamp)
  (make-process
   :name "use-metadata"
   :command (learn-ocaml-command-constructor
             :token token
             :server server
             :command "set-options"
             )
   :stderr learn-ocaml-log-buffer
   :sentinel (apply-partially
              #'learn-ocaml-error-handler
              nil
              callback)))

(defun learn-ocaml-create-token (nickname secret callback)
  "Creates a new token."
  (learn-ocaml-print-time-stamp)
  (let ((buffer (generate-new-buffer "create-token")))
    (make-process
     :name "create-token"
     :command (learn-ocaml-command-constructor
               :command "create-token"
               :param1 nickname
               :param2 secret
               )
     :stderr learn-ocaml-log-buffer
     :buffer buffer
     :sentinel (apply-partially
                #'learn-ocaml-error-handler
                buffer
                (lambda (s)
                  (funcall-interactively
                   callback
                   (replace-regexp-in-string "\n\\'" "" s)))))))

(defun learn-ocaml-give-exercice-list (callback)
  "Gives to the callback function a json containing the exercise list"
  (learn-ocaml-print-time-stamp)
  (let ((buffer (generate-new-buffer "exercise-list")))
    (make-process
     :name "exercise-list"
     :command (learn-ocaml-command-constructor
	       :command "exercise-list")
     :stderr learn-ocaml-log-buffer
     :buffer buffer
     :sentinel (apply-partially
		#'learn-ocaml-error-handler
		buffer
		(lambda (s)
		  (funcall-interactively
		   callback (json-read-from-string s)))))))

;;
;; Wrappers
;;

(defun learn-ocaml-show-questions (id)
  "Display to the default browser the questions concerning
the exercise with id equal to id"
  (interactive)
  (learn-ocaml-give-server
   (lambda (server)
     (learn-ocaml-give-token
      (lambda (token)
	;;very important if you don't do it you risk to open eww
	(setq browse-url-browser-function 'browse-url-default-browser) 
	(funcall #'browse-url (concat server "/description.html#id=" id "&token=" token)))))))
			    
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
   :id id
   :callback (lambda (_) (message-box "File(s) downloaded correctly"))))

(defun learn-ocaml-download-template-wrapper (id)
  (interactive `(,(let ((input (read-string(concat
                                            "Enter the id of the exercise [default "
                                            learn-ocaml-exercise-id
                                            " ]: "))))
                    (if (string-equal "" input)
                        learn-ocaml-exercise-id
                      input
                      ))))
  (learn-ocaml-download-template
   :id id
   :callback (lambda (_) (message-box "Template downloaded correctly"))))

;;;###autoload
(defun learn-ocaml-grade-wrapper()
  (interactive)
  (learn-ocaml-grade-file
   :id learn-ocaml-exercise-id
   :file buffer-file-name
   :callback (lambda (_)
	       (setq browse-url-browser-function 'browse-url-default-browser) 
	       (browse-url learn-ocaml-temp))))

;
;exercise list diplay 
;

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
			     (learn-ocaml-download-template-wrapper id))
		   "Download template")
    (widget-insert " ")
    (widget-create 'learn-ocaml-button
		   :notify (lambda (&rest ignore)
			     (learn-ocaml-download-server-file-wrapper id))
		   "Download last upladed version")
    (widget-insert " ")
    (widget-create 'learn-ocaml-button
		   :notify (lambda (&rest ignore)
			     (find-file (concat id ".ml")))
		   "Open corresponding local file")
    (widget-insert " ")
    (widget-create 'learn-ocaml-button
		   :notify (lambda (&rest ignore)
			     (learn-ocaml-show-questions id))
		   "Open questions")
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
  "Displays the exercise list of the configured server"
  (switch-to-buffer "*learn-ocaml-exercise-list*")
  (kill-all-local-variables)
  (let ((inhibit-read-only t))
    (erase-buffer))  
  (remove-overlays)
  (learn-ocaml-print-groups " " json)
  (use-local-map widget-keymap)
  (widget-setup)) 

(defun learn-ocaml-display-exercise-list ()
  ""
  (interactive)
  (learn-ocaml-give-exercice-list
   (lambda (brut-json)
     (learn-ocaml-display-exercise-list-to-wrap (elt (elt brut-json 0) 1)))))

;;
;; on-load management
;;

(defun learn-ocaml-on-load-to-wrap (token server callback)
  ""
  (let* ((after-questions (lambda (new-server-value new-token-value )
			   (learn-ocaml-use-metadata
			    new-token-value
			    new-server-value
			    (lambda (_)
			      (funcall callback) 
			      (learn-ocaml-show-metadata)))))
	(new-server-value (if (not(string-equal server ""))
                              nil
                            (message-box "No server found please enter the server")
                            (read-string "Enter server: ")))
        (new-token-value (cl-destructuring-bind (token-phrase use-found-token use-another-token)
                             (if (not (string-equal token ""))
                                 `(,(concat "Token found:  " token) ("Use found token" . 0) ("Use another token" . 1))
                               '("No token found " "Use found token" ("Use existing token" . 1)))
                         (case (x-popup-dialog
                                t `(,(concat token-phrase " \n What do you want to do ? \n")
                                    ,use-found-token
                                    ,use-another-token
                                    ("Create-new-token" . 2)))
                           (0 nil)
                           (1 (read-string "Enter token: "))
                           (2 (let ((nickname (read-string "What nickname you want to use for the token ? "))
				     (secret (read-string "What secret do you want to associate to this token? ")))
			       (funcall
				#'learn-ocaml-create-token
				nickname
				secret
				(apply-partially after-questions new-server-value))
                              'creating))))))
	(unless (eq new-token-value 'creating)
	  (funcall after-questions new-server-value new-token-value))))


(defun learn-ocaml-on-load-wrapped (callback)
  "Function to execute when loading the mode."
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
    (define-key map "\C-as" #'learn-ocaml-show-metadata)
    (define-key map [menu-bar] nil)
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
    ["Download template" learn-ocaml-download-template-wrapper]
    ["Show exercise list" learn-ocaml-display-exercise-list]
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
  (force-mode-line-update)) ;; a random instruction is needed to update menu bar

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
  :lighter " Learnocaml"
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
	       (setq default-directory
		(read-string  "Choose your working directory :" default-directory))
	       (learn-ocaml-display-exercise-list))))
	  (setq learn-ocaml-loaded t)))
    (setq learn-ocaml-loaded nil)
    (remove-hook 'caml-mode-hook #'learn-ocaml-mode)
    (remove-hook 'tuareg-mode-hook #'learn-ocaml-mode)))

(provide 'learn-ocaml)

;;; learn-ocaml.el ends here
