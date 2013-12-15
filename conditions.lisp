;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.http.implementation; -*-
;;; Copyright 2013 [james anderson](mailto:james.anderson@setf.de) All Rights Reserved

(in-package :de.setf.http.implementation)

(define-condition http:condition (simple-condition)
  ((code
    :initform -1
    :reader http:condition-code)
   (text
    :initform "" :initarg :text
    :reader http:condition-text)
   (message
    :initarg :message :initform nil
    :reader http:condition-message))
  (:report report-condition))

(define-condition http:error (http:condition simple-error)
  ())

(defmacro def-condition (name classes slots &rest options)
  (let ((code (getf (rest (assoc 'code slots)) :initform)))
    `(progn (defun ,name (&rest args)
              (declare (dynamic-extent args))
              (apply #'error ',name args))
            ,@(when code
                `((defconstant ,name ,code)))
            (define-condition ,name ,classes ,slots ,@options))))

(defmethod http:report-condition-headers ((condition http:condition) (response t))
  (setf (http:response-status-code response) (http:condition-code condition)))

(def-condition http:redirect (http:condition)
  ((code :initform 301 :initarg :code)
   (text :initform "Redirect")
   (location
    :initarg :location :initform (error "location is required.")
    :reader http:condition-location)))

(define-compiler-macro http:redirect (&whole form &rest args)
  "iff the arguments begin with something other than a keyword, take them as
 a redirection continuation, signal the redirection with that function instead
 of a location with the presumption, that there is a handler."
  (if (keywordp (first args))
    form
    `(error 'http:redirect :location (lambda () ,@args))))

(def-condition http:ok (http:condition)
  ((code :initform 200 :allocation :class)
   (text :initform nil :allocation :class)))

(def-condition http:no-content (http:condition)
  ((code :initform 204 :allocation :class)
   (text :initform "No Content" :allocation :class)))

(def-condition http::not-modified (http:condition)
  ((code :initform 304 :allocation :class)
   (text :initform "Not Modified" :allocation :class)
   (etag 
    :initarg :etag :initform (error "etag is required.")
    :reader http:condition-etag)
   (mtime
    :initarg :mtime :initform (error "mtime is required")
    :reader http:condition-mtime)))

(def-condition http:bad-request (http:condition)
  ((code :initform 400 :allocation :class)
   (text :initform "Bad Request" :allocation :class)))

(def-condition http:unauthorized (http:condition)
  ((code :initform 401 :allocation :class)
   (text :initform "Unauthorized" :allocation :class)))

(def-condition http:not-found (http:condition)
  ((code :initform 404 :allocation :class)
   (text :initform "Not Found" :allocation :class)))

(def-condition http:not-acceptable (http:condition)
  ((code :initform 406 :allocation :class)
   (text :initform "Not Acceptable" :allocation :class)))

(def-condition http:not-allowed (http:condition)
  ((code :initform 405 :allocation :class)
   (text :initform "Not Allowed" :allocation :class)))

(def-condition http:length-required (http:error)
  ((code :initform 411 :allocation :class)
   (text :initform "Length Required" :allocation :class)))

(def-condition http::precondition-failed (http:error)
  ((code :initform 412 :allocation :class)
   (text :initform "Precondition Failed" :allocation :class)))

(def-condition http:request-too-large (http:error)
  ((code :initform 413 :allocation :class)
   (text :initform "Request Entity Too Large" :allocation :class)))

(def-condition http::unsupported-media-type (http:error)
  ((code :initform 415 :allocation :class)
   (text :initform "Unsupported Media Type" :allocation :class)))

(def-condition http:internal-error (http:error)
  ((code :initform 500 :allocation :class)
   (text :initform "Internal Server Error" :allocation :class)
   (retry-after
    :initform nil :initarg :retry-after
    :reader http:condition-retry-after)))

(def-condition http:not-implemented (http:error)
  ((code :initform 501 :allocation :class)
   (text :initform "Not Implemented" :allocation :class)))

(def-condition http::bad-gateway (http:error)
  ((code :initform 502 :allocation :class)
   (test :initform "Bad Gateway" :allocation :class)))

(def-condition http::service-unavailable (http:error)
  ((code :initform 503 :allocation :class)
   (test :initform "Service Unavailable" :allocation :class)))

(def-condition http::gateway-timeout (http:error)
  ((code :initform 504 :allocation :class)
   (test :initform "Gateway Timeout" :allocation :class)))

(def-condition http::http-version-not-supported (http:error)
  ((code :initform 505 :allocation :class)
   (test :initform "HTTP Version Not SUpported" :allocation :class)))


;;;

(defmethod report-condition ((condition http:condition) stream)
    (format stream "~s ~a~@[ (~a)~]~%"
            (http:condition-code condition)
            (http:condition-text condition)
            (http:condition-message condition)))

