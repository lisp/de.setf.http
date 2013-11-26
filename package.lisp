;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: common-lisp-user; -*-
;;; Copyright 2013 [james anderson](mailto:james.anderson@setf.de) All Rights Reserved

(in-package :common-lisp-user)


(defpackage :http-method-package
  (:use ))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(:get :put :head :patch :post :delete :options :trace :connect) :http-method-package))


(defpackage :de.setf.http
  (:nicknames :http)
  (:use )
  #+:hunchentoot
  (:import-from :hunchentoot
               :*acceptor*
               :*request*
               :*reply*
               :*session*
               :+http-ok+
               :process-connection
               :request-class)
  (:export :*acceptor*
           :*log-level*
           :*request*
           :*response*
           :*session*
           :+http-ok+
           :acceptor
           :acceptor-dispatch-function
           :acceptor-request-class
           :acceptor-response-class
           :agent
           :agent-authentication-form
           :agent-id
           :agent-is-admin
           :authenticate-request-password
           :authenticate-request-token
           :authenticate-request-session
           :authorize-request
           :bad-request
           :bind-resource
           :class-resource-keywords
           :class-resource-pattern
           :condition
           :condition-code
           :condition-etag
           :condition-location
           :condition-message
           :condition-mtime
           :condition-retry-after
           :condition-text
           :decode-request
           :def-resource-function
           :define-dispatch-method
           :dispatch-function
           :encode-response
           :error
           :function-method-keys
           :function-package
           :function-resource-classes
           :function-resource-class
           :function-resource-function-class
           :http
           :length-required
           :log
           :log-trace
           :log-debug
           :log-info 
           :log-notice
           :log-warn
           :log-error
           :log-critical
           :log-fatal
           :no-content
           :not-modified
           :not-found
           :not-acceptable
           :not-allowed
           :make-request
           :make-response
           :ok
           :redirect
           :report-condition-headers
           :request
           :request-accept-header
           :request-accept-type
           :request-acceptor
           :request-auth-token
           :request-authentication
           :request-class
           :request-content-stream
           :request-content-type
           :request-content-type-header
           :request-effective-method
           :request-header
           :request-line-method
           :request-method
           :request-path
           :request-post-argument
           :request-session-id
           :request-too-large
           :request-session-cookie-name
           :resource
           :resource-authorization-list
           :resource-class
           :resource-function
           :resource-function-p
           :resource-path
           :resource-pattern
           :respond-to-request
           :response
           :response-acceptor
           :response-character-encoding-header
           :response-class
           :response-content-stream
           :response-content-type-header
           :response-close-stream-p
           :response-headers-sent-p
           :response-keep-alive-p
           :response-location-header
           :response-media-type-header
           :response-protocol
           :response-retry-after-header
           :response-request
           :response-status-code
           :response-www-authenticate-header
           :send-entity-body
           :send-headers
           :unauthorized
           :unsupported-content-type
           :internal-error
           :not-implemented
           ))

(defpackage :de.setf.http.implementation
  (:use :common-lisp
        :de.setf.utility
        ;; :de.setf.utility.codecs
        #+sbcl :sb-cltl2))


