;;;; cl-curl.asd

(asdf:defsystem #:trivial-web
  :serial t
  :depends-on (:usocket
	       :quri
	       :alexandria
	       :cl-ppcre)
  :components ((:file "package")
               (:file "cl-curl")))

