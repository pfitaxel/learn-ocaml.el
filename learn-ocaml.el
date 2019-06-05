(defgroup learn-ocaml nil
  "learn-ocaml  in Emacs "
  :prefix "learn-ocaml-")

(defconst learn-ocaml-version "0.0.1")

(defconst learn-ocaml-command-name "learn-ocaml-client" )

(defconst learn-ocaml-temp "~/.learnocaml-temp.html")

(setq learn-ocaml-log-buffer (get-buffer-create "*log*"))


(require 'browse-url )
(require 'cl)
(require 'cl-lib)

(cl-defun learn-ocaml-command-constructor (&key token server fetch id set-options no-html print-token file)
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
	(print-token-option (if print-token
			   "--print-token"
			   nil))	
	
	(list (list learn-ocaml-command-name token-option server-option fetch-option id-option set-options-option no-html-option print-token-option file))
	)
    
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

