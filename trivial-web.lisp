;;;; cl-curl.lisp

(in-package #:trivial-web)

(defun http-protocolp (url)
  (scan "^http[s]?://*" url))

(defun make-http-url (url)
  (if (http-protocolp url)
      url
      (concatenate 'string "http://" url)))

(defun http-pull (url)
  (let* ((parsed-url (if (http-protocolp url)
			 url
			 (make-http-url url)
			 ))
	 (the-uri (uri parsed-url)))
    (if (uri-path the-uri)
	the-uri
	(setf (uri-path the-uri) "/"))
    (let ((sock (socket-connect (uri-host the-uri) 80))
		(get-req (format nil "GET ~a HTTP/1.1~C~CHost: ~a~C~CConnection: close~C~CAccept: */*~C~C~C~C"
				 (uri-path the-uri)
				 #\return #\newline
				 (uri-host the-uri)
				 #\return #\newline
				 #\return #\newline
				 #\return #\newline
				 #\return #\newline
				 )))
      (format (socket-stream sock) get-req)
      (force-output (socket-stream sock))
      (wait-for-input sock :timeout 10)
      (let ((str (read-stream-content-into-string (socket-stream sock))))
	(socket-close sock)
	(values str get-req)))))
