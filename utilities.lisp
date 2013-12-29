;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.http.implementation; -*-
;;; Copyright 2013 [james anderson](mailto:james.anderson@setf.de) All Rights Reserved

(in-package :de.setf.http.implementation)

;;; read-only state

(declaim (ftype (function () http:acceptor) http:acceptor))
(defun http:request () http:*request*)
(define-compiler-macro http:request () 'http:*request*)
(defun http:resource () http:*resource*)
(define-compiler-macro http:resource () 'http:*resource*)
(defun http:response () http:*response*)
(define-compiler-macro http:response () 'http:*response*)

(defun http-verb-p (object)
  (and (keywordp object)
       (eq (find-symbol (symbol-name object) *http-method-package*) object)))

(defun http-verb-list-p (object)
  (and (consp object)
       (every #'http-verb-p object)))

(defun log-level-qualifies? (level)
  (find level (member http:*log-level* *log-levels*)))

(defun call-if-log-level-qualifies (level operator)
  "Given a log LEVEL and an OPERATOR, call the operator iff the log level is satisfied."
  (declare (dynamic-extent operator))
  (when (log-level-qualifies? level)
    (funcall operator)))

(macrolet ((def-log-op (level)
             `(progn
                (defmacro ,(intern (concatenate 'string (string :log-) (string level)) :http) (destination format-control &rest args)
                  `(flet ((call-log ()
                            (http:log ,,level ,destination ,format-control ,@args)
                            t))
                     (declare (dynamic-extent (function call-log)))
                     (call-if-log-level-qualifies ,,level (function call-log)))))))
  (def-log-op :fatal)
  (def-log-op :critical)
  (def-log-op :error)
  (def-log-op :warn)
  (def-log-op :notice)
  (def-log-op :info)
  (def-log-op :debug))

(defgeneric http:log (level destination format-control &rest arguments)
  (:documentation
    "Emit a log message to the destination.")

  (:method (level (destination stream) format-control &rest arguments)
    (declare (dynamic-extent arguments))
    (format destination "~&;;; [~a] ~?" level format-control arguments))
  )


;;;
;;; media type support
;;; implement media type unions and resolution

;;; unions

(let ((mime-token '(:greedy-repetition 1 nil (:inverted-char-class #\, #\; #\=)))
      (mime-major-minor '(:sequence
                          (:register (:greedy-repetition 1 nil (:inverted-char-class #\/)))
                          #\/
                          (:register (:greedy-repetition 1 nil (:inverted-char-class #\, #\;)))))
      (mime-q '(:sequence (:char-class #\q #\q) #\= (:register (:greedy-repetition 1 nil (:char-class #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\.)))))
      (mime-accept-params '(:sequence #\; (:alternation mime-q mime-token)))
      (mime-range-and-parameters '(:sequence mime-major-minor (:greedy-repetition 0 nil mime-accept-params))))
                                   
  (setf (cl-ppcre:parse-tree-synonym 'mime-major-minor) mime-major-minor)
  (setf (cl-ppcre:parse-tree-synonym 'mime-token) mime-token)
  (setf (cl-ppcre:parse-tree-synonym 'mime-q) mime-q)
  (setf (cl-ppcre:parse-tree-synonym 'mime-accept-params) mime-accept-params)
  (setf (cl-ppcre:parse-tree-synonym 'mime-range-and-parameters) mime-range-and-parameters))


(defparameter *content-coding-pattern* (cl-ppcre:create-scanner "([^,;=]+)(?:;q=([^,]*))?"))
(defparameter *accept-range-pattern* (cl-ppcre:create-scanner 'mime-range-and-parameters))

;;; (cl-ppcre:scan-to-strings *accept-range-pattern* "text/html")
;;; (cl-ppcre:scan-to-strings *accept-range-pattern* "text/html;q=1;a=b")
;;; (cl-ppcre:scan-to-strings *accept-range-pattern* "text/html,application/json,application/rdf+xml")
;;; (cl-ppcre:scan-to-strings *accept-range-pattern* "text/html;q=0.2,application/json,application/rdf+xml")
;;; (cl-ppcre:scan-to-strings *content-coding-pattern* "gzip;q=0.2,identity")

(defun parse-media-range (range)
  (coerce (nth-value 1 (cl-ppcre:scan-to-strings *accept-range-pattern* range)) 'list))

(defun parse-content-coding (coding)
  (coerce (nth-value 1 (cl-ppcre:scan-to-strings *content-coding-pattern* coding)) 'list))

(defun qvalue-char-p (c)
  (or (digit-char-p c) (eql c #\.)))

(defun parse-qvalue (qvalue)
  (if (every #'qvalue-char-p qvalue)
    (read-from-string qvalue)
    (http:bad-request :message (format nil "Invalid qvalue: '~a'" qvalue))))


(defun compute-accept-ordered-types (header)
  (let* ((accept-ranges (split-string header #\,))
         (types (loop for range in accept-ranges
                        for (major minor q) = (or (parse-media-range range)
                                                  (http:bad-request :message (format nil "Invalid accept range: ~s." range)))
                        for type = (dsu:intern-mime-type-key (format nil "~a/~a" major minor) :if-does-not-exist nil)
                        for quality = (cond (q (parse-qvalue q))
                                            (t 1))
                        if type
                        collect (cons type quality)
                        else
                        do (http:log-warn (http:acceptor) "The mime type '~a/~a' is not defined." major minor))))
    (if types
      (mapcar #'first (sort types #'> :key #'rest))
      (http:not-acceptable "Unacceptable accept ranges: '~a'" header))))

(defun compute-accept-encoding-ordered-codings (header)
  ;; translate the string into a sorted, qualified a-list
  (stable-sort (loop for element in (split-string (remove #\space header) ",")
                     for (content-coding qvalue) = (parse-content-coding element)
                     if content-coding
                     collect (cons content-coding (if qvalue (parse-qvalue qvalue) 1))
                     else do (http:bad-request :message (format nil "Invalid content coding: ~s." element)))
               #'> :key #'rest))
  


;;; (compute-accept-ordered-types "text/html")
;;; (compute-accept-ordered-types "*/*")
;;; (parse-content-coding "compress")
;;; (compute-accept-encoding-ordered-codings "compress, gzip")
;;; (compute-accept-encoding-ordered-codings nil)
;;; (compute-accept-encoding-ordered-codings "")
;;; (compute-accept-encoding-ordered-codings "*")
;;; (compute-accept-encoding-ordered-codings "compress;q=0.5, gzip;q=1.0")
;;; (compute-accept-encoding-ordered-codings "gzip;q=1.0, identity; q=0.5, *;q=0")




(defgeneric intern-media-type (accept-header)
  (:method ((header string))
    (let* ((ordered-types (compute-accept-ordered-types header))
           (class-name (intern (format nil "~{~a~^+~}" ordered-types) :mime))
           (class (or (find-class class-name nil)
                      (c2mop:ensure-class class-name :direct-superclasses ordered-types))))
      (make-instance class))))


(defun concrete-media-type (mime-type)
  "return an instance of the initial class in the union precedence list"
  (symbol-value (class-name (first (c2mop:class-direct-superclasses (class-of mime-type))))))

;;; codecs

(defgeneric copy-stream (input-stream output-stream)
  (:method ((input-stream stream) (output-stream stream))
    (let ((buffer (make-array 4096 :element-type (stream-element-type input-stream))))
      (loop for length = (read-sequence buffer input-stream)
            while (plusp length)
            do (write-sequence buffer output-stream :end length))))
  
  (:method ((input-stream stream) (output pathname))
    (with-open-file (output-stream output :direction :output :if-exists :supersede :if-does-not-exist :create
                                   :element-type 'unsigned-byte)
      (copy-stream input-stream output-stream))))

(defgeneric http:encode-response (content response content-type)
  (:documentation "Implements the default behavior for resource function
    encoding methods when they are declared without a body. the default
    methods delegate to send-entity-body.")

  (:method ((content null) (response t) (content-type t)))

  (:method ((content string) (response t) (content-type t))
    (http:send-entity-body response content))

  (:method ((content vector) (response t) (content-type t))
    (http:send-entity-body response content))

  (:method ((content t) (response t) (content-type mime:text/plain))
    ;; this should send out the entity body chunked
    (format (http:response-content-stream response) "~a" content))

  (:method ((content-stream stream) (response t) (content-type t))
    "the default method given a stream result is to just copy the stream. that is,
     given any standard media type, presume the result generator handles the
     respective serialization entirely and the stream content is correct as-is."
    (unwind-protect (copy-stream content-stream (http:response-content-stream response))
      (close content-stream))))


(defgeneric http:decode-request (resource request content-type)
  (:documentation "Implements the default behavior for resource function
    encoding methods when they are declared without a body. the default
    methods delegate to send-entity-body."))


;;; times

(defparameter *rdf1123-date-time-scanner*
  (cl-ppcre:create-scanner "([a-zA-z]{3}), ([0-9]{2}) ([a-zA-z]{3}) ([0-9]{4}) ([0-9]{2}):([0-9]{2}):([0-9]{2}) GMT"))
;;; (nth-value 1 (cl-ppcre:scan-to-strings *rdf1123-date-time-scanner* "Wed, 30 May 2007 18:47:52 GMT"))

(defun http:parse-rfc1123 (value &key junk-allowed)
  (let ((substrings (coerce (nth-value 1 (cl-ppcre:scan-to-strings *rdf1123-date-time-scanner* value)) 'list)))
    (if substrings
      (destructuring-bind (wkday day month year hour minute second)
                          substrings
        (declare (ignore wkday))
        (encode-universal-time (parse-integer second)
                               (parse-integer minute)
                               (parse-integer hour)
                               (parse-integer day)
                               (date:decode-month-name month)
                               (parse-integer year)
                               0))
      (unless junk-allowed
        (error "Invalid rdf1123 date-time: ~s." value)))))

(defgeneric http:encode-rfc1123 (value &optional stream)
  (:method ((value integer) &optional (stream nil))
    (multiple-value-bind (second minute hour day month year weekday)
                         (decode-universal-time value 0)
      (format stream "~a, ~d ~:(~a~) ~4,'0d ~2,'0d:~2,'0d:~2,'0d GMT"
              (date:day-in-week-name weekday 3) day (date:month-name month 3) year hour minute second))))
