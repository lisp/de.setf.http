;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.http.implementation; -*-
;;; Copyright 2013 [james anderson](mailto:james.anderson@setf.de) All Rights Reserved

(in-package :de.setf.http.implementation)


(declaim (special http:*request* http::*reply*))

(defparameter http:*content-initial-length* 1024
  "The initial byte count of a request entity body content for request which specify no content length.
 (default 1024)")

(defparameter http:*content-length-limit* nil
  "When non-nulll, constrains the byte count of a request entity body content. If nil, no constaint applies.
 (default nil)")

(defparameter *keyword-marker-character* #\:
  "Specifies the initial character which marks keyword elements in a pattern. #\: is the default")

(defparameter http:*request* nil
  "Bound to the request instance for the dynamic extent of the respond-to-request call.")

(defparameter http:*resource* nil
  "Bound to the request's resource instance for the dynamic extent of the resource function call.")

(defparameter http:*response* nil
  "Bound to the response instance for the dynamic extent of the respond-to-request call.")

(defparameter *http-method-package* (find-package :http-method-package))

(defparameter *log-condition* nil)

(defparameter *log-destination* :syslog
  "specifies where to write log entries: :syslog or a stream instance")

(defparameter http:*log-levels* '(:trace :debug :info :notice :warn :error :critical :fatal))

(defparameter http:*log-level* :debug
  "The current log level. Used by the log-* operators to filter log entries.")

(defparameter http:+method-keys+ '(:get :put :head :patch :post :delete :options :trace :connect)
  "The names for known http operations. These limit the resource function methods
 which will be combined in an effective response method. see add-method(http:resource-function)
 and the http method combination.")


(defparameter http:*user-agent-properties*
  `((,(cl-ppcre:create-scanner "^mozilla/.*" :case-insensitive-mode t)
     :interactive t))
  "an a-list of ppcre patterns to match user agent strings which indicate interactive agents
 and mapping to the respective properties.")
