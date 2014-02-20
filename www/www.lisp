;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: cl-user; -*-

(in-package :cl-user)

(defconstant +page-content+ "
<html>
 <head>
  <title>http test page</title>
  </head>
 <body>
  <h1>test page</h1>
  <p>This is a test for '~a'.</p>
  </body>
 </html>
")

(http:def-resource (/test/* "/test/([^/]+)" :target) ()
  ((target
    :initform nil :initarg :target
    :reader resource-target)))

(http:def-resource-function respond (resource request response)
  (:get ((resource /test/*) (request t) (response t) (content-type t) (accept-type t))
        (resource-target resource))
  (:encode ((resource t) (request t) (response http:response) (content-type t) (accept-type mime:text/html))
    (let ((content (call-next-method)))
      (http:send-headers response)
      (http:log :debug *trace-output* +page-content+ content)
      (write-sequence (format nil  +page-content+ content) (http:response-content-stream response)))))


(defpackage "127.0.0.1"
  (:use )
  (:import-from :cl-user :respond)
  (:export :respond))

;;#+(or)
(progn
 (trace tbnl:process-connection de.setf.http.implementation::ensure-dispatch-function )
 (trace http:bind-resource de.setf.http.implementation::compute-effective-resource-function-method de.setf.http.implementation::intern-media-type)
 (trace de.setf.http.implementation::compute-dispatch-methods http:function-package)
 )

(defparameter *a* (make-instance 'tbnl::tbnl-acceptor :port 8009 :address "127.0.0.1"))
(setf (http:acceptor-dispatch-function *a*) "127.0.0.1")
(hunchentoot:start *a*)

;;; (compute-dispatch-methods cl-user::*a*)


          
      