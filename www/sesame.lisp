;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: org.datagraph.spocq.server; -*-
;;;  Copyright 2013 [james anderson](mailto:james.anderson@setf.de) All Rights Reserved

(in-package :org.datagraph.spocq.server)

(:documentation "implement the sparql sesame2 protocol

This file implements ths responses to the documented 'HTTP communication protocol for Sesame 2'
(http://openrdf.callimachus.net/sesame/2.7/docs/system.docbook?view#chapter-http-protocol),
whereby operations defined in the graph store protocol are delegated to its response operator.
 
        ${STORE_URL}/${STORE_ACCOUNT}
          /protocol              : protocol version (GET)
          /repositories          : overview of available repositories (GET)
          /${STORE_REPOSITORY}   : query evaluation and administration tasks on 
                                   a repository (GET/POST/DELETE)
            /statements          : repository statements (GET/POST/PUT/DELETE)
            /contexts            : context overview (GET)
            /size                : #statements in repository (GET)
            /rdf-graphs          : named graphs overview (GET)
                /service         : Graph Store operations on indirectly referenced named graphs 
                                   in repository (GET/PUT/POST/DELETE)
                                   includes the query argument graph=${STORE_IGRAPH}
                /${STORE_RGRAPH} : Graph Store operations on directly referenced named graphs 
                                   in repository (GET/PUT/POST/DELETE)
            /namespaces          : overview of namespace definitions (GET/DELETE)
                /${STORE_PREFIX} : namespace-prefix definition (GET/PUT/DELETE)

")

(defconstant +sesame-version+ "6")


;;;
;;; resources

(http:def-resource (/* "/([^/]+)" :account-name)
                   ()
  ((account-name
    :initarg :account-name :initform (error "account-name is required.")
    :reader resource-account-name)))

(http:def-resource (/*/protocol "/([^/]+)/protocol" account-name)
                   (/*)
  ())

(http:def-resource (/*/repositories "/([^/]+)/repositories" account-name)
                   (/*)
  ())

(http:def-resource (/*/repositories/* "/([^/]+)/repositories/([^/]+)"
                                      :account-name :repository-name)
                   (/*/repositories)
  ((repository-name
    :initarg :repository-name :initform (error "repository-name is required.")
    :accessor resource-repository-name)))

(http:def-resource (/*/repositories/*/statements "/([^/]+)/repositories/([^/]+)/statements"
                                                 :account-name :repository-name)
                   (/*/repositories/*)
  ())

(http:def-resource (/*/repositories/*/contexts "/([^/]+)/repositories/([^/]+)/contexts"
                                               :account-name :repository-name)
                   (/*/repositories/*) () )

(http:def-resource (/*/repositories/*/size "/([^/]+)/repositories/([^/]+)/contexts"
                                           :account-name :repository-name)
                   (/*/repositories/*)
  ())

(http:def-resource (/*/repositories/*/namespaces "/([^/]+)/repositories/([^/]+)/namespaces"
                                                 account-name repository-name)
                   (/*/repositories/*)
  ())

(http:def-resource (/*/repositories/*/namespaces/* "/([^/]+)/repositories/([^/]+)/namespaces/([^/]+)"
                                                 :account-name :repository-name :prefix)
                   (/*/repositories/*/namespaces)
  ((prefix
    :initarg :prefix :initform (error "prefix is required.")
    :accessor resource-prefix)))

(http:def-resource (/*/repositories/*/rdf-graphs "/([^/]+)/repositories/([^/]+)/rdf-graphs"
                                                 :account-name :repository-name)
                   (/*/repositories/*)
  ())

(http:def-resource (/*/repositories/*/rdf-graphs/service "/([^/]+)/repositories/([^/]+)/rdf-graphs/service"
                                                         :account-name :repository-name)
                   (/*/repositories/*/rdf-graphs)
  ())

(http:def-resource (/*/repositories/*/rdf-graphs/service/* "/([^/]+)/repositories/([^/]+)/rdf-graphs/service/([^/]+)"
                                                         :account-name :repository-name :graph-name)
                   (/*/repositories/*/rdf-graphs/service)
  ((graph-name
    :initarg :graph-name :initform (error "graph-name is required.")
    :accessor resource-graph-name)))


;;;
;;; response implementations

(http:def-resource-function sesame-protocol-id (resource request response)
  (:post-processing text/plain :charset "UTF-8")
  (:get ((resource /*/protocol) request response)
    (setf (response-content-disposition response) "protocol.txt" t)
    (write-string +sesame-version+ (http:response-content-stream response))))


(http:def-resource-function sesame-repository-list (resource request response)
  (:documentation "Return a list of the account repositories with access 
    information respective the request agent.")

  (:auth :identification authenticate-request-password)
  (:auth :identification authenticate-request-token)
  (:auth :identification authenticate-request-session)

  (:post-processing mime:application/sparql-results+json)
  (:post-processing mime:application/sparql-results+xml)

  (:get ((resource /*/repositories) request response)
    (let ((repositories (account-repositories (resource-account resource))))
      (make-solution-field :dimensions '(?:id ?:uri ?:title ?:readable ?:writable)
                           :solutions (loop (loop with agent = (request-agent request)
                                                  for repository in repositories
                                                  collect (list (repository-name repository)
                                                                (repository-uri repository)
                                                                (repository-title repository)
                                                                (if (resource-readable-p repository agent) "true" "false")
                                                                (if (resource-writable-p repository agent) "true" "false"))))))))


(http:def-resource-function sesame-repository (resource request response)
  (:auth :identification authenticate-request-password)
  (:auth :identification authenticate-request-token)
  (:auth :identification authenticate-request-session)

  (:post-processing mime:application/sparql-results+json)
  (:post-processing mime:application/sparql-results+xml)
  
  (:get ((resource /*/repositories/*) request response))
  (:post ((resource /*/repositories/*) request response))
  (:delete ((resource /*/repositories/*) request response)))


(http:def-resource-function sesame-response (resource request response)
  (:documentation
    "Respond to requests for repository metadata: 
     contexts, namespaces, size are implemented directly.
     statements operations relegate to the graph store implementation.")

  (:auth :identification authenticate-request-password)
  (:auth :identification authenticate-request-token)
  (:auth :identification authenticate-request-session)

  (:post mime:application/sparql-results+json)
  (:post mime:application/sparql-results+xml)
  (:post ((resource t) (request t) (response t) (content-type t) (accept-type mime:text/plain))
    (let ((result (call-next-method)))
      (begin-response response)
      (format (response-content-stream response) "~a~a" result +crlf+)))

  (:get ((resource /*/repositories) request response)
    "Return a list the accounts repositories"
    (let ((repositories (account-repositories (resource-account resource))))
      (make-solution-field :dimensions '(?:id ?:uri ?:title ?:readable ?:writable)
                           :solutions (loop for repository in repositories
                                            collect (list (repository-id repository)
                                                          (iri-lexical-form (repository-uri repository))
                                                          (first-line (repository-description repository))
                                                          (if (access-authorized (request-agent request) repository <acl:read>) spocq:|true| spocq:|false|)
                                                          (if (access-authorized (request-agent request) repository <acl:write>) spocq:|true| spocq:|false|))))))

  (:get ((resource /*/repositories/*) request response content-type accept-type)
    (graph-store-response resource request response content-type accept-type))

  (:post ((resource /*/repositories/*) request response content-type accept-type)
    (graph-store-response resource request response content-type accept-type))

  (:delete ((resource /*/repositories/*) request response)
    (http:not-implemented))
  
  (:get ((resource /*/repositories/*/statements) request response content-type accept-type)
    (graph-store-response resource request response content-type accept-type))

  (:post ((resource /*/repositories/*/statements) request response content-type accept-type)
    (graph-store-response resource request response content-type accept-type))

  (:put ((resource /*/repositories/*/statements) request response content-type accept-type)
    (graph-store-response resource request response content-type accept-type))

  (:delete ((resource /*/repositories/*/statements) request response content-type accept-type)
    (graph-store-response resource request response content-type accept-type))

  (:get ((resource /*/repositories/*/contexts) request response)
     (multiple-value-bind (solutions dimensions)
                          (run-sparql "SELECT DISTINCT ?contextID WHERE {GRAPH ?contextID {?s ?p ?o}}"
                                      :repository (resource-repository resource))
       (make-solution-field :dimensions dimensions
                            :solutions solutions)))

  (:get ((resource /*/repositories/*/size) request response)
    (let ((size (repository-statement-count (resource-repository resource))))
      (make-solution-field :dimensions '(?:size)
                           :solutions `((size)))))

  (:get ((resource /*/repositories/*/namespaces) request response)
    (let ((bindings (namespace-bindings (resource-repository resource))))
      (make-solution-field :dimensions '(?:prefix ?:namespace)
                           :solutions (loop for (prefix . iri) in bindings
                                            collect (list prefix (iri-lexical-form iri))))))

  (:delete ((resource /*/repositories/*/namespaces) request response)
    (http:not-implemented))

  (:get ((resource /*/repositories/*/namespaces/*) request response (content-type mime:text/plain))
    ;; specify the content type explicitly as it differs from the rest,
    ;; emit the response and indicate completion
    (let* ((bindings (namespace-bindings (resource-repository resource)))
           (prefix (resource-prefix resource))
           (namespace-iri (rest (assoc prefix bindings))))
      (cond (namespace-iri
             (begin-response response)
             (iri-lexical-form namespace-iri))
            (t
             (http:not-found :message (format nil "Undefined prefix: ~a" prefix))))))

  (:put ((resource /*/repositories/*/namespaces/*) request response)
    (http:not-implemented))

  (:delete ((resource /*/repositories/*/namespaces/*) request response)
    (http:not-implemented)))


(http:def-resource-function sesame-service-response (resource request response)
  (:auth :identification authenticate-request-password)
  (:auth :identification authenticate-request-token)
  (:auth :identification authenticate-request-session)

  (:post-processing mime:application/sparql-results+json)
  (:post-processing mime:application/sparql-results+xml)

  (:get ((resource /*/repositories/*/rdf-graphs) request response content-type accept-type)
    (return-from sesame-service-response
      (sesame-response (clone-resource resource '/*/repositories/*/contexts) request response content-type accept-type)))

  (:get ((resource /*/repositories/*/rdfgraphs/service) request response content-type accept-type)
    (return-from sesame-service-response
      (graph-store-response (clone-resource resource '/*/*) request response content-type accept-type)))

  (:post ((resource /*/repositories/*/rdfgraphs/service) request response content-type accept-type)
    (return-from sesame-service-response
      (graph-store-response (clone-resource resource '/*/*) request response content-type accept-type)))

  (:put ((resource /*/repositories/*/rdfgraphs/service) request response content-type accept-type)
    (return-from sesame-service-response
      (graph-store-response (clone-resource resource '/*/*) request response content-type accept-type)))

  (:delete ((resource /*/repositories/*/rdfgraphs/service) request response content-type accept-type)
    (return-from sesame-service-response
      (graph-store-response (clone-resource resource '/*/*) request response content-type accept-type)))

  (:get ((resource /*/repositories/*/rdfgraphs/*) request response content-type accept-type)
    (return-from sesame-service-response
      (graph-store-response (clone-resource resource '/*/*) request response content-type accept-type)))

  (:post ((resource /*/repositories/*/rdfgraphs/*) request response content-type accept-type)
    (return-from sesame-service-response
      (graph-store-response (clone-resource resource '/*/*) request response content-type accept-type)))

  (:put ((resource /*/repositories/*/rdfgraphs/*) request response content-type accept-type)
    (return-from sesame-service-response
      (graph-store-response (clone-resource resource '/*/*) request response content-type accept-type)))

  (:delete ((resource /*/repositories/*/rdfgraphs/*) request response content-type accept-type)
    (return-from sesame-service-response
      (graph-store-response (clone-resource resource '/*/*) request response content-type accept-type))))
