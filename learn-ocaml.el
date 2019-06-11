(defgroup learn-ocaml nil
  "learn-ocaml  in Emacs "
  :prefix "learn-ocaml-")

(defconst learn-ocaml-version "0.0.1")

(defconst learn-ocaml-command-name "learn-ocaml-client" )

(defconst learn-ocaml-temp "~/.learnocaml-temp.html")

(defvar learn-ocaml-log-buffer (get-buffer-create "*learn-ocaml-log*"))


(require 'browse-url )
(require 'cl)
(require 'cl-lib)


(cl-defun learn-ocaml-command-constructor (&key token server fetch id set-options no-html print-token dont-submit file)
  (let* ((token-option (if token
			  (concat "--token=" token)
			nil))
	(server-option (if server
			   (concat "--server=" server)
			 nil))
	(fetch-option (if fetch
			  "--fetch"
			nil))	
	(id-option (if id
		       (concat "--id=" id)
		     nil))
		   
	(set-options-option (if set-options
			   "--set-options"
			 nil))
	(no-html-option (if no-html
			   nil
			 "--html"))
	(dont-submit-option (if dont-submit
			   "-n"
			 nil))
	(print-token-option (if print-token
			   "--print-token"
			   nil))
	
	(list (list learn-ocaml-command-name token-option server-option fetch-option id-option set-options-option no-html-option print-token-option dont-submit-option file)))
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
		 :fetch t)
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
		 )
       :stderr learn-ocaml-log-buffer
       :filter #'learn-ocaml-file-writter-filter
       :sentinel #'(lambda (proc string) "" (interactive)
		     (if (string-equal string "finished\n")
		       (browse-url-firefox learn-ocaml-temp )))))

