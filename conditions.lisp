;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.http.implementation; -*-
;;; Copyright 2013 [james anderson](mailto:james.anderson@setf.de) All Rights Reserved

(in-package :de.setf.http.implementation)


(define-condition http:condition (simple-condition)
  ((code
    :initform -1
    :reader http:condition-code)
   (text
    :initform "" :initarg :text
    :reader http:condition-text))
  (:report report-condition)
  (:default-initargs :format-control nil :format-arguments ())
  (:documentation "The abstract http condition class specializes simple-condition
    and adds slots for the status code and the status text. It expects
    specializations to provide a format control string and to specialize
    simple-condition-format arguments respective any arguments to be
    presented to format.
    Conditions which appropriately curtail or supplant the processing can be
    signaled, in which case the information is emitted in lieu of any other
    response. The report for any others, when signalled during content
    processing, is recorded in the log and appended to the in-progress body.
    In addition, the condition name is bound as a constant to the respective
    status code value."))


(defmethod report-condition ((condition http:condition) stream)
  (format stream "HTTP Status: ~s (~a)~@[: ~?~]"
          (http:condition-code condition)
          (http:condition-text condition)
          (simple-condition-format-control condition)
          (simple-condition-format-arguments condition)))

(define-condition http:error (http:condition simple-error)
  ())

;;; some conditions prescribe specific parameters. these are created like
;;;  (http:not-modified :etag "tag" :mtime (g-u-t))
;;; and specialize the reporting argument construction or the entire
;;; reposting method. others provide a default message as the format
;;; string and permit it to be supplanted, as
;;;  (http:bad-request "parsing syntax error ...")
;;; others need no additional information, as in
;;;  (http:unauthorized)

(defmacro def-condition (name classes slots &rest options)
  (let ((code (getf (rest (assoc 'code slots)) :initform)))
    `(progn (defun ,name (&optional format-control &rest args)
              (declare (dynamic-extent args))
              (etypecase format-control
                (keyword (apply #'error format-control args))
                (string (error (apply #'make-condition ',name
                                      :format-control format-control
                                      :format-arguments args)))
                (null (error ',name))))
            ,@(when code
                `((defconstant ,name ,code)))
            (define-condition ,name ,classes ,slots ,@options))))

(defmethod http:report-condition-headers ((condition http:condition) (response t))
  (setf (http:response-status-code response) (http:condition-code condition)))

(def-condition http:continue (http:condition)
  ((code :initform 100 :allocation :class)
   (text :initform "Continue" :allocation :class)))

(def-condition http:switching-protocols (http:condition)
  ((code :initform 101 :allocation :class)
   (text :initform "Switching Protocols" :allocation :class)))

(def-condition http:ok (http:condition)
  ((code :initform 200 :allocation :class)
   (text :initform "OK" :allocation :class)))

(def-condition http:created (http:condition)
  ((code :initform 201 :allocation :class)
   (text :initform "Created" :allocation :class)))

(def-condition http:accepted (http:condition)
  ((code :initform 202 :allocation :class)
   (text :initform "Accepted" :allocation :class)))

(def-condition http:no-content (http:condition)
  ((code :initform 204 :allocation :class)
   (text :initform "No Content" :allocation :class)))

(def-condition http:reset-content (http:condition)
  ((code :initform 205 :allocation :class)
   (text :initform "Reset Content" :allocation :class)))

(def-condition http:partial-content (http:condition)
  ((code :initform 206 :allocation :class)
   (text :initform "Partial Content" :allocation :class)))

(def-condition redirection-condition (http:condition)
  ((location
    :initarg :location :initform (error "location is required.")
    :reader http:condition-location))
  (:default-initargs :format-control "The resource has moved to ~a."))

(defmethod http:report-condition-headers ((condition redirection-condition) (response t))
  (setf (http:response-location response) (http:condition-location condition)))
(defmethod simple-condition-format-arguments ((condition redirection-condition))
  (list (http:condition-location condition)))

(def-condition http:multiple-choices (redirection-condition)
  ((code :initform 300 :allocation :class)
   (text :initform "Multiple Choices" :allocation :class)
   (location-choices
    :initarg :location-choices :initform (error "location-choices is required.")
    :reader http:condition-location-choices)))

(defmethod simple-condition-format-arguments ((condition http:multiple-choices))
  (list (http:condition-location condition)
        (http:condition-location-choices condition)))

(def-condition http:moved-permanently (redirection-condition)
  ((code :initform 301 :initarg :code)
   (text :initform "Moved Permanently")))

(define-compiler-macro http:moved-permanently (&whole form &rest args)
  "iff the arguments begin with something other than a keyword, take them as
 a redirection continuation, signal the redirection with that function instead
 of a location resource with the presumption, that there is a handler."
  (if (keywordp (first args))
    form
    `(error 'http:moved-permanently :location (lambda () ,@args))))

(def-condition http:found (redirection-condition)
  ((code :initform 302 :initarg :code)
   (text :initform "Found")))

(def-condition http:see-other (redirection-condition)
  ((code :initform 303 :initarg :code)
   (text :initform "See Other")))

(def-condition http::not-modified (http:condition)
  ((code :initform 304 :allocation :class)
   (text :initform "Not Modified" :allocation :class)
   (etag 
    :initarg :etag :initform (error "etag is required.")
    :reader http:condition-etag)
   (mtime
    :initarg :mtime :initform (error "mtime is required")
    :reader http:condition-mtime)))
(defmethod http:report-condition-headers ((condition http:not-modified) (response t))
  (setf (http:response-etag response) (http:condition-etag condition))
  (setf (http:response-date response) (http:condition-mtime condition)))

(def-condition http:use-proxy (redirection-condition)
  ((code :initform 305 :allocation :class)
   (text :initform "Use Proxy" :allocation :class)))

(def-condition http:temporary-redirect (redirection-condition)
  ((code :initform 307 :allocation :class)
   (text :initform "Temporary Redirect" :allocation :class)))

(def-condition http:bad-request (http:condition)
  ((code :initform 400 :allocation :class)
   (text :initform "Bad Request" :allocation :class)))

(def-condition http:unauthorized (http:condition)
  ((code :initform 401 :allocation :class)
   (text :initform "Unauthorized" :allocation :class)))

(def-condition http:payment-required (http:condition)
  ((code :initform 402 :allocation :class)
   (text :initform "Payment Required" :allocation :class)))

(def-condition http:forbidden (http:condition)
  ((code :initform 403 :allocation :class)
   (text :initform "Forbidden" :allocation :class)))

(def-condition http:not-found (http:condition)
  ((code :initform 404 :allocation :class)
   (text :initform "Not Found" :allocation :class)))

(def-condition http:method-not-allowed (http:condition)
  ((code :initform 405 :allocation :class)
   (text :initform "Method Not Allowed" :allocation :class)))

(def-condition http:not-acceptable (http:condition)
  ((code :initform 406 :allocation :class)
   (text :initform "Not Acceptable" :allocation :class)))

(def-condition http:proxy-authentication-required (http:condition)
  ((code :initform 407 :allocation :class)
   (text :initform "Proxy Authentication Required" :allocation :class)))

(def-condition http:request-timeout (http:condition)
  ((code :initform 408 :allocation :class)
   (text :initform "Request Timeout" :allocation :class)))

(def-condition http:conflict (http:condition)
  ((code :initform 409 :allocation :class)
   (text :initform "Conflict" :allocation :class)))

(def-condition http:gone (http:condition)
  ((code :initform 410 :allocation :class)
   (text :initform "Gone" :allocation :class)))

(def-condition http:length-required (http:error)
  ((code :initform 411 :allocation :class)
   (text :initform "Length Required" :allocation :class)))

(def-condition http:precondition-failed (http:error)
  ((code :initform 412 :allocation :class)
   (text :initform "Precondition Failed" :allocation :class)))

(def-condition http:request-entity-too-large (http:error)
  ((code :initform 413 :allocation :class)
   (text :initform "Request Entity Too Large" :allocation :class)))

(def-condition http:request-uri-too-long (http:error)
  ((code :initform 414 :allocation :class)
   (text :initform "Request URI Too Long" :allocation :class)))

(def-condition http:unsupported-media-type (http:error)
  ((code :initform 415 :allocation :class)
   (text :initform "Unsupported Media Type" :allocation :class)))

(def-condition http:requested-range-not-satisfiable (http:error)
  ((code :initform 416 :allocation :class)
   (text :initform "Requested Range Not Satisfiable" :allocation :class)))

(def-condition http:expectation-failed (http:error)
  ((code :initform 417 :allocation :class)
   (text :initform "Expectation Failed" :allocation :class)))

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
