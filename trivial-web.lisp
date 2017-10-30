;;;; cl-curl.lisp

(in-package #:trivial-web)

(define-condition bad-url (error)
  ((the-url :initarg :the-url :reader the-url)))

(defmacro constantp (var)
  (let ((old-var (gensym)))))

(defmacro definedp (var)
  `(handler-case 
      (if ',var
	  ;; this is to handle nil in the macro
	  ;; otherwise nil calls type error
	  ;; when checking the symbol value
	  ;; which is of course nil
	  (symbol-value ',var)
	  t)
    (unbound-variable () nil)
    (type-error () ',var)))


(defun protocolp (regex url)
  (not (not (scan regex url))))

(defun http-protocolp (url)
  (protocolp "^http[s]?://*" url))

(defun make-http-url (url)
  (if (http-protocolp url)
      url
      (concatenate 'string "http://" url)))

(eval-when (:compile-toplevel :execute :load-toplevel)

  (defun str->ip-vec (ip)
    (handler-case 
	(let ((tmp (dotted-quad-to-vector-quad ip)))
	  (remove-if #'(lambda (x) (not (and x (<= 0 x)))) tmp))
      (sb-int:simple-parse-error () nil))) 

  (defun real-ipp (ip)
    (cond
      ((stringp ip) (= 4 (length (str->ip-vec ip))))
      ((typep ip 'sequence) (= 4 (length ip)))
      (t nil))))

(defmacro http-pull (url)
  (cond
    ((and ))
    ((stringp url)
     (let ((parsed-url (gensym "p-url"))
	   (sock (gensym "sock"))
	   (the-uri (gensym "uri")))
       `(let* ((,parsed-url (if (http-protocolp ,url)
				,url
				(make-http-url ,url)
				))
	       (,the-uri (uri ,parsed-url)))
	  (if (uri-path ,the-uri)
	      ,the-uri
	      (setf (uri-path ,the-uri) "/"))
	  (let ((,sock (socket-connect (uri-host ,the-uri) 80))
		(get-req (format nil "GET ~a HTTP/1.1~C~CHost: ~a~C~CConnection: close~C~CAccept: */*~C~C~C~C"
				 (uri-path ,the-uri)
				 #\return #\newline
				 (uri-host ,the-uri)
				 #\return #\newline
				 #\return #\newline
				 #\return #\newline
				 #\return #\newline
				 )))
	    (format (socket-stream ,sock) get-req)
	    (force-output (socket-stream ,sock))
	    (wait-for-input ,sock :timeout 10)
	    (let ((addr (get-peer-name ,sock)) (str (read-stream-content-into-string (socket-stream ,sock))))
	      (socket-close ,sock)
	      (values str get-req addr))))))
    ))

(defun ftp-connect (url)
  21)


