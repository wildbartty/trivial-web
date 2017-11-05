;;;; cl-curl.asd

(asdf:defsystem #:trivial-web
  :serial t
  :depends-on (:usocket ;Handles lowlevel stuff
	       :quri    ;Manupulates urls
	       :alexandria ;General utilities
	       ;; :drakma     
	       :cl-ftp     ;Handles ftp
	       :cl-smtp    ;Smtp
	       :cl-pop     ;pop
	       :plump      ;
	       :snmp
	       :cl-mime
	       :cl-ppcre)
  :components ((:file "package")
               (:file "trivial-web")))

