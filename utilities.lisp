;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.http.implementation; -*-
;;; Copyright 2013 [james anderson](mailto:james.anderson@setf.de) All Rights Reserved

(in-package :de.setf.http.implementation)

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


(defparameter *accept-range-pattern* (cl-ppcre:create-scanner 'mime-range-and-parameters))

;;; (cl-ppcre:scan-to-strings *accept-range-pattern* "text/html")
;;; (cl-ppcre:scan-to-strings *accept-range-pattern* "text/html;q=1;a=b")
;;; (cl-ppcre:scan-to-strings *accept-range-pattern* "text/html,application/json,application/rdf+xml")
;;; (cl-ppcre:scan-to-strings *accept-range-pattern* "text/html;q=2,application/json,application/rdf+xml")

(defun parse-media-range (range)
  (coerce (nth-value 1 (cl-ppcre:scan-to-strings *accept-range-pattern* range)) 'list))

(defun compute-accept-ordered-types (header)
  (let* ((accept-ranges (split-string header #\,))
         (types (loop for range in accept-ranges
                        for (major minor q) = (or (parse-media-range range)
                                                  (http:bad-request :message (format nil "Invalid accept range: ~s." range)))
                        for type = (dsu:intern-mime-type-key (format nil "~a/~a" major minor))
                        for quality = (cond (q
                                             (unless (every #'digit-char-p q)
                                               (http:bad-request :message "Invalid accept field: '~a'" header))
                                             (parse-integer q))
                                            (t
                                             1))
                        if type
                        collect (cons type quality)
                        else
                        do (http:log-warn (http:acceptor) "The mime type '~a/~a' is not defined." major minor))))
    (if types
      (mapcar #'first (sort types #'> :key #'rest))
      (http:not-acceptable "Unacceptable accept ranges: '~a'" header))))

;;; (compute-accept-ordered-types "text/html")




(defgeneric intern-media-type (accept-header accept-charset)
  (:method (header (accept-charset-header null))
    (intern-media-type header :utf-8))

  (:method ((header string) (accept-charset t))
    (let* ((ordered-types (compute-accept-ordered-types header))
           (class-name (intern (format nil "~{~a~^+~}" ordered-types) :mime))
           (class (or (find-class class-name)
                      (c2mop:ensure-class class-name :direct-superclasses ordered-types))))
      (make-instance class :charset accept-charset))))


(defun concrete-media-type (mime-type)
  "return an instance of the initial class in the union precedence list"
  (symbol-value (class-name (first (c2mop:class-direct-superclasses (class-of mime-type))))))

;;; codecs

(defgeneric http:encode-response (content response content-type)
  (:method ((content null) (response t) (content-type t)))
  (:method ((content t) (response t) (content-type mime:text/plain))
    (format (http:response-content-stream response) "~a" content)))

(defgeneric http:decode-request (request content-type)
  )


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