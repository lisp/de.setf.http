;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: common-lisp-user; -*-
;;; Copyright 2013 [james anderson](mailto:james.anderson@setf.de) All Rights Reserved

(in-package :common-lisp-user)


(defpackage :http-method-package
  (:use )
  (:documentation
    "The home package for HTTP verbs"))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(:get :put :head :patch :post :delete :options :trace :connect) :http-method-package))


(defpackage :de.setf.http
  (:nicknames :http)
  (:export :*log-level*
           :*request*
           :*resource*
           :*response*
           :*session*
           :+http-ok+
           :+method-keys+
           :acceptor
           :acceptor-dispatch-function
           :acceptor-request-class
           :acceptor-response-class
           :agent
           :agent-authentication-form
           :agent-id
           :agent-is-admin
           :anonymous-resource-p
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
           :copy-stream
           :decode-request
           :def-resource-function
           :def-resource
           :define-dispatch-method
           :dispatch-function
           :encode-response
           :encode-rfc1123
           :error
           :function-method-keys
           :function-package
           :function-resource-classes
           :function-resource-class
           :function-resource-function-class
           :http
           :input-stream
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
           :output-stream
           :parse-rfc1123
           :redirect
           :report-condition-headers
           :request
           :request-accept-charset
           :request-accept-content-encoding
           :request-accept-header
           :request-accept-type
           :request-acceptor
           :request-agent
           :request-auth-token
           :request-authentication
           :request-cache-matched-p
           :request-class
           :request-content-length
           :request-content-stream
           :request-content-type-header
           :request-effective-method
           :request-etags
           :request-header
           :request-host
           :request-if-modified-since
           :request-media-type
           :request-method
           :request-negotiated-character-encoding
           :request-negotiated-content-encoding
           :request-original-method
           :request-path
           :request-post-argument
           :request-post-arguments
           :request-query-argument
           :request-query-arguments
           :request-remote-ip-address
           :request-session-id
           :request-too-large
           :request-session-cookie-name
           :request-unmodified-since
           :resource
           :resource-authorization-list
           :resource-class
           :resource-function
           :resource-function-p
           :resource-identifier
           :resource-path
           :resource-pattern
           :resource-request
           :resource-request-argument
           :respond-to-option-request
           :respond-to-request
           :response
           :response-acceptor
           :response-accept-encoding
           :response-accept-ranges
           :response-allow
           :response-allow-header
           :response-cache-control
           :response-character-encoding
           :response-class
           :response-compute-media-type
           :response-content-disposition
           :response-content-encoding
           :response-content-length
           :response-content-length-header
           :response-content-stream
           :response-content-type-header
           :response-close-stream-p
           :response-etag
           :response-headers-unsent-p
           :response-keep-alive-p
           :response-last-modified
           :response-location-header
           :response-media-type
           :response-protocol
           :response-retry-after-header
           :response-request
           :response-socket-stream
           :response-status-code
           :response-transfer-encoding-header
           :response-vary
           :response-www-authenticate-header
           :send-entity-body
           :send-headers
           :stream
           :stream-media-type
           :unauthorized
           :unsupported-content-type
           :internal-error
           :not-implemented
           ))

(defpackage :de.setf.http.implementation
  (:nicknames :http.i)
  (:use :common-lisp
        :de.setf.utility
        ;; :de.setf.utility.codecs
        :chunga                         ; for chunking operator names
        :trivial-gray-streams           ; for stream operator names
        #+sbcl :sb-cltl2))



