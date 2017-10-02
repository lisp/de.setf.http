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
  (:use )
  (:export :*content-initial-length*
           :*content-length-limit*
           :*log-level*
           :*request*
           :*resource*
           :*response*
           :*session*
           :*user-agent-properties*
           :+method-keys+
           :accepted
           :acceptor
           :acceptor-address
           :acceptor-dispatch-function
           :acceptor-request-class
           :acceptor-response-class
           :agent
           :agent-authentication-form
           :agent-id
           :agent-is-admin
           :anonymous-resource-p
           :authenticate-anonymous
           :authenticate-request-location
           :authenticate-request-password
           :authenticate-request-session
           :authenticate-request-token
           :authorize-request
           :bad-gateway
           :bad-request
           :bind-resource
           :class-resource-keywords
           :class-resource-pattern
           :clear-headers
           :conflict
           :condition
           :condition-code
           :condition-etag
           :condition-location
           :condition-location-choices
           :condition-message
           :condition-mtime
           :condition-retry-after
           :condition-text
           :continue
           :copy-stream
           :created
           :decode-request
           :def-resource-function
           :def-resource
           :define-dispatch-method
           :dispatch-function
           :encode-response
           :encode-rfc1123
           :error
           :expectation-failed
           :forbidden
           :found
           :function-default-accept-header
           :function-method-keys
           :function-package
           :function-patterns
           :function-resource-class
           :function-resource-function-class
           :gateway-timeout
           :gone
           :handle-condition
           :http
           :http-version-not-supported
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
           :method-not-allowed
           :moved-permanently
           :no-content
           :not-modified
           :not-found
           :not-acceptable
           :make-request
           :make-resource
           :make-response
           :moved-permanently
           :multiple-choices
           :not-modified
           :ok
           :output-stream
           :parse-rfc1123
           :partial-content
           :payment-required
           :precondition-failed
           :proxy-authentication-required
           :report-condition
           :report-condition-body
           :report-condition-headers
           :request
           :request-accept-charset
           :request-accept-content-encoding
           :request-accept-header
           :request-accept-type
           :request-acceptor
           :request-agent
           :request-argument-list
           :request-auth-token
           :request-authentication
           :request-body
           :request-cache-matched-p
           :request-class
           :request-content-length
           :request-content-stream
           :request-content-type-header
           :request-effective-method
           :request-entity-too-large
           :request-etags
           :request-header
           :request-headers
           :request-host
           :request-if-modified-since
           :request-is-interactive
           :request-keep-alive-p
           :request-media-type
           :request-method
           :request-negotiated-character-encoding
           :request-negotiated-content-encoding
           :request-original-method
           :request-path
           :request-post-argument
           :request-post-arguments
           :request-post-argument-list
           :request-query-argument
           :request-query-arguments
           :request-query-argument-list
           :request-remote-ip-address
           :request-response
           :request-session-id
           :request-session-cookie-name
           :request-timeout
           :request-unmodified-since
           :request-uri
           :request-uri-host-name
           :request-uri-too-long
           :request-user-agent
           :request-property
           :requested-range-not-satisfiable
           :reset-content
           :resource
           :resource-file-type
           :resource-function
           :resource-function-p
           :resource-path
           :resource-pattern
           :resource-pattern-class
           :resource-pattern-name
           :resource-pattern-path
           :resource-pattern-predicate
           :resource-pattern-subpatterns
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
           :response-date
           :response-etag
           :response-header
           :response-headers
           :response-headers-sent-p
           :response-keep-alive-p
           :response-last-modified
           :response-location
           :response-media-type
           :response-protocol
           :response-retry-after-header
           :response-request
           :response-socket-stream
           :response-status-code
           :response-transfer-encoding-header
           :response-vary
           :response-www-authenticate-header
           :see-other
           :send-condition
           :send-entity-body
           :send-headers
           :service-unavailable
           :start
           :stream
           :stream-header-stream
           :stream-media-type
           :switching-protocols
           :temporary-redirect
           :unauthorized
           :unsupported-media-type
           :use-proxy
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



