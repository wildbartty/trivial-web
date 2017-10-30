;;;; cl-curl.asd

(asdf:defsystem #:trivial-web
  :serial t
  :depends-on (:usocket
	       :quri
	       :alexandria
	       :drakma
	       :cl-ftp
	       :cl-smtp
	       :cl-pop
	       :plump
	       :snmp
	       :cl-mime
	       :cl-ppcre)
  :components ((:file "package")
               (:file "trivial-web")))

