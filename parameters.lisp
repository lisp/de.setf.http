;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.http.implementation; -*-
;;; Copyright 2013 [james anderson](mailto:james.anderson@setf.de) All Rights Reserved

(in-package :de.setf.http.implementation)


(declaim (special http:*request* http::*reply*))

(defparameter http:*response* nil)

(defparameter *http-method-package* (find-package :http-method-package))

(defparameter *log-levels* '(:trace :debug :info :notice :warn :error :critical :fatal))

(defparameter http:*log-level* :debug ;; :warn
  "The current log level. Used by the log-* operators to filter log entries.")

