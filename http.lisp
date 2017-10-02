;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.http.implementation; -*-
;;; Copyright 2013 [james anderson](mailto:james.anderson@setf.de) All Rights Reserved

(in-package :de.setf.http.implementation)

(:documentation "a modular http implementation:

 This file is an abstract implementation of the protocol aspects of an HTTP
 server in terms of a half-dozen classes: 
   http:acceptor
   http:request
   http:response
   http:resource
   http:resource-function
   http:agent
 three operators
   http:encode-response
   http:decode-request
 a condition complement corresponding to HTTP response status codes, and the
 method-combination:
   http:http

 In order to create a running server, these are combined with a wire protocol
 and event implementation, such as hunchentoot or cl-fastcgi, and the 
 concrete methods for individual resources. 
")


(defclass http:acceptor ()
  ((http:dispatch-function
    :initform nil :initarg :dispatch-function
    :reader http:acceptor-dispatch-function
    :writer setf-acceptor-dispatch-function
    :documentation
    "An acceptor invokes this function for each request path. It interns the
    path and dispatches to the combination of path, request and response
    instances to the concrete implementation. Where no value is provided, the
    acceptor defines a generic operator with dispatch methods for each
    implementation method found.")
   (header-instances
    :initform (make-hash-table :test 'equalp)
    :reader acceptor-header-instances))
  (:documentation
    "The acceptor class implements pattern-based http request path dispatching
    based on methods implemented in the package which corresponds to the
    acceptor's host name."))



(defclass http:agent ()
  ((id
    :initarg :id :initform (error "id is required.")
    :reader http:agent-id)
   (is-admin
    :initarg :is-admin :initform nil
    :reader http:agent-is-admin)
   (authentication-form
    :initarg :authentication-form :initform nil
    :accessor http:agent-authentication-form)))


(defclass http:dispatch-function (standard-generic-function)
  ((patterns
    :initform nil :initarg :patterns
    :accessor http:function-patterns
    :documentation
    "Binds a list of the top resource patterns as the discrimination
    graph root when interning request resource paths.")
   (resource-class
    :initform 'http:resource :initarg :resource-class
    :accessor http:function-resource-class
    :documentation
    "A dispatch function binds those request handlers which are specialized for
     resources of this class.")
   (resource-function-class
    :initform 'http:resource-function :initarg :resource-function-class
    :accessor http:function-resource-function-class
    :documentation
    "A dispatch function binds request handlers which are found as methods of
    generic function of this class.")
   (package
    :initform *package* :initarg :package
    :accessor http:function-package))
  (:metaclass c2mop:funcallable-standard-class)
  (:documentation
    "The class of function computed by an acceptor to implement its resource
    dispatch.
     By default, the dispatch function will bind all resource methods for
    http:resource, found in functions of type http:resource-function in
    bound to symbols in the current package."))


(defclass http:request ()
  ((response
    :initform nil :initarg :response
    :accessor http:request-response     ; allow setting due to instantiation order
    :documentation "the response respective this active request.")
   (agent
    :initform nil :initarg :agent
    :accessor http:request-agent
    :documentation "Provides for an agent instance to be determined by any
     authorization methods. If no methods are defined, this remains null.")
   (session-cookie-name
    :initform nil :initarg :session-cookie-name
    :accessor http:request-session-cookie-name)
   (method
     :initform nil
     :reader get-request-method :writer setf-request-method
     :documentation
     "Binds the effective request method resective over-riding headers.
     (See http:request-method)")
   (media-type
    :initarg :media-type
    :accessor request-media-type 
    :documentation "Binds the concrete media type for request content.
     The initial state is unbound and the mime type instance is computed upon first reference from the
     content-type header value. (See http:request-media-type - note the package)")
   (accept-type
    :initform nil
    :accessor http:request-accept-type
    :documentation "Binds the combined media type which is computed from the
     request accept header and accept-charset header upon first reference together
     with the media type encodings defined for the matched response function.
     This is computed on-the-fly, at the point of response function invocation and then used to
     determine the applicable methods.")
   (negotiated-content-encoding
    ;; no initform
    :reader get-request-negotiated-content-encoding :writer setf-request-negotiated-content-encoding
    :documentation "Caches the content encoding negotiated from the combination
     of the request accept-encoding header and the available encoding list provided
     to negotiated-content-encoding."))
  (:documentation
    "Each request is represented with an instance of this class. It serves as
     a protocol class and to extend the respective class from any concrete
     implementation with slots support generic protocol mechanisms.
     The base implementation must provide access to headers and the content
     stream"))


(defclass http:resource ()
  ((request
    :initarg :request
    :accessor http:resource-request
    :documentation "caches the respective request for access to request parameters")
   (path
    :initarg :path
    :reader http:resource-path
    :documentation "the path components of the resource identifer which
     contribute to its classification."))
  (:documentation "The abstract class of request resources.
   Each request derives a concrete resource subclass and intantiates it with the
   respective properties as matched from the request uri. (see http:bind-resource)
   The resource caches the respective request - and is therefor not a singleton,
   as well as the path string. Other fields may be added to concrete sub-classes.
   It does not cache the request iri additional to the path.
   The complete original is present in the request."))


(defclass http:resource-function (standard-generic-function)
  ((method-keys
    :initform '() :initarg :method-keys
    :accessor http:function-method-keys
    :documentation "Collects the http method keys present in the function's methods. This limits
     the scope of the generated effective methods.")
   (default-accept-header
     :initform "*/*"
     :type string
     :accessor http:function-default-accept-header
     :documentation "The accept header value to be used when a request either includes no accept header
      or includes just */*.The default value, whichis itself */* is effective only if the function actually
      implements a method for that effective accept type.
      It should be a string in order that it is matched with the actual encode methods which are defined
      for the function.")
   (accept-types
    :initform ()
    :accessor resource-function-accept-types
    :documentation "An a-list map from canonicalized accept header to the combined media type to be supplied
     as the accept type in function calls to the operator. It is maintained as a simple association list.
     For a moderate accept header count and allowing that conflicts updating lead to recomputing, it
     avoids a lock."))
  (:metaclass c2mop:funcallable-standard-class)
  (:documentation
    "The class of generic functions intended to provide methods which implement
    responses to http requests. Each method provides for a single combination of
    resource pattern, request content and accept content types, combined with
    request and response instances to carry individual request state.

    The expected method combination is http."))

(defgeneric http:resource-function-p (function)
  (:method ((function http:resource-function))
    t)
  (:method ((object t))
    nil))


(defclass http:resource-pattern ()
  ((name
    :initarg :name :initform (error "name is required.")
    :reader http:resource-pattern-name)
   (path
    :reader http:resource-pattern-path)
   (subpatterns
    :initarg :subpatterns :initform nil
    :accessor http:resource-pattern-subpatterns)
   (class
    :initarg :class :initform (error "class is required.")
    :reader http:resource-pattern-class)
   (predicate
    :reader http:resource-pattern-predicate)))


(defclass http:response ()
  ((acceptor
    :initarg :acceptor
    :reader http:response-acceptor)
   (request
    :initform nil :initarg :request
    :accessor http:response-request     ; allow setting due to instantiation order
    :documentation "the request respective this active response.")
   (state
    :initform nil
    :accessor response-state)
   (content-stream
    :initarg :content-stream :initform nil ;; some error handlers make one w/o args (error "content-stream is required.")
    :reader get-response-content-stream :writer (setf http:response-content-stream)
    :documentation "The response stream is instantiated on-demand to emit the
    response body. It supports both chunking and character encoding with the
    settings taken from the respective headers at the time it is first used.")
   (media-type
    :initarg :media-type :initform nil
    :reader http:response-media-type :writer setf-response-media-type
    :documentation "Caches the intended media type (incl character encoding) to
     be used to configure the response content stream.")
   (protocol
    :initarg :server-protocol :initarg :protocol
    :accessor http:response-protocol)
   (close-stream-p
    :initform nil :initarg :close-stream-p
    :accessor http:response-close-stream-p))
  (:documentation
    "Each response is represented with an instance of this class. It serves as
     a protocol class and to extend the respective class from any concrete
     implementation with slots support generic protocol mechanisms.
     The base implementation must provide access to headers and the content
     stream"))


;;;
;;; class protocols


(defmethod initialize-instance ((instance http:acceptor) &key)
  ;; just a place-holder stub
  (call-next-method))

(defgeneric http:start (acceptor)
  (:method ((acceptor http:acceptor))
    (assert (http:acceptor-dispatch-function acceptor) ()
            "A dispatch function is required.")))

(defgeneric (setf http:acceptor-dispatch-function) (function acceptor &key address dispatch-function-class name resource-function-class)
  (:method ((function function) (acceptor http:acceptor) &key &allow-other-keys)
    (setf-acceptor-dispatch-function function acceptor))

  (:method ((package-name string) (acceptor http:acceptor) &rest args)
    (apply #'(setf http:acceptor-dispatch-function) (or (find-package package-name) (error "Invalid acceptor package: ~s." package-name))
           acceptor
           args))

  (:method ((package package) (acceptor http:acceptor) &key (address (or (http:acceptor-address acceptor)
                                                                         (error "Acceptor address is required: ~s." acceptor)))
            (dispatch-function-class 'http:dispatch-function)
            (resource-function-class 'http:resource-function)
            (name (intern address package)))
    (setf (http:acceptor-dispatch-function acceptor)
          (apply #'ensure-dispatch-function name
                 `(,@(when dispatch-function-class
                       `(:generic-function-class ,dispatch-function-class))
                   ,@(when resource-function-class
                       `(:resource-function-class ,resource-function-class))
                   :package ,package)))))


;;;
;;; operators


;;;
;;; acceptor

(defgeneric http:acceptor-address (acceptor)
  )

(defgeneric http:make-request (context &rest initargs)
  (:documentation
    "Make a REQUEST instance for the ACCEPTOR, setting up those slots
  that are determined from the SOCKET by calling the appropriate
  socket query functions.")

  (:method ((context http:acceptor) &rest initargs)
    (declare (dynamic-extent initargs))
    (apply #'http:make-request (http:acceptor-request-class context)
           :acceptor context
           initargs))
  (:method ((class-name symbol) &rest initargs)
    (declare (dynamic-extent initargs))
    (apply #'http:make-request (find-class class-name) initargs))
  (:method ((class standard-class) &rest initargs)
    (declare (dynamic-extent initargs))
    (apply #'make-instance class initargs)))

(defgeneric http:acceptor-request-class (acceptor)
  )

(defgeneric http:make-response (context &rest initargs)
  (:method ((context http:acceptor) &rest initargs)
    (declare (dynamic-extent initargs))
    (apply #'http:make-response (http:acceptor-response-class context)
           :acceptor context
           initargs))
  (:method ((class-name symbol) &rest initargs)
    (declare (dynamic-extent initargs))
    (apply #'http:make-response (find-class class-name) initargs))
  (:method ((class standard-class) &rest initargs)
    (declare (dynamic-extent initargs))
    (apply #'make-instance class initargs)))

(defgeneric http:acceptor-response-class (acceptor)
  )


(defgeneric http:define-dispatch-method (dispatch-function handler-name resource-type)
  (:argument-precedence-order resource-type dispatch-function handler-name)

  (:method ((function http:dispatch-function) (handler-name symbol) (resource-type symbol))
    (http:define-dispatch-method function handler-name (find-class resource-type)))
  
  (:method ((acceptor http:acceptor) (handler-name t) (resource-class t))
    (http:define-dispatch-method (http:acceptor-dispatch-function acceptor) handler-name resource-class))
  
  (:method ((function http:dispatch-function) (handler-name symbol) (resource-class class))
    "Generate a dispatch method for the respective resource class which will invoke the function which includes
     the method for that class. Indirect through funcall-resource-function, which derives the composite accept
     type from the accept header"
    (let ((t-class (find-class t)))
      (c2mop:finalize-inheritance resource-class)
      (c2mop:ensure-method function
                           `(lambda (resource request response)
                              (funcall-resource-function ',handler-name resource
                                                         request response
                                                         (http:request-media-type request) (http:request-accept-header request)))
                           :qualifiers '()
                           :lambda-list '(resource request response)
                           :specializers (list resource-class t-class t-class)))))


(defgeneric http:respond-to-request (acceptor request response)
  (:documentation
    "Given an active acceptor and the respective request and response instances,
    apply the request to the acceptor's resource and generte a response")

  (:method ((acceptor http:acceptor) request response)
    "The generic resource acceptor invokes its generated dispatch function."
    (funcall (http:acceptor-dispatch-function acceptor)
             (http:request-path request)
             request
             response))
  (:method :after ((acceptor http:acceptor) (request t) (response t))
    (setf (response-state response) :complete)))


(defgeneric http:handle-condition (acceptor condition)
  (:documentation "Invoked for any 'unknown' condition type which is signaled during a process-connection
    invocation. Can either resignal or decline to handle - as is the case for the default method, in which
    case the connection processing implementation will resignal as an http:internal-error.")
  (:method ((acceptor t) (condition t))
    ;; decline to handle
    ))

;;;
;;; dispatch function

(defmethod add-method :after ((function http:dispatch-function) method)
  "As a side effect of adding a method, integrate its path pattern into
 the generic function's resource discrimination network."
  (let ((class (first (c2mop:method-specializers method))))
    (when (subtypep class 'http:resource)
      (add-pattern function (make-instance 'http:resource-pattern :class class)))))


(defgeneric add-pattern (function pattern)
  (:documentation "Add a pattern to the tree already known to the function.")

  (:method ((function http:dispatch-function) (new-pattern http:resource-pattern))
    (setf (http:function-patterns function)
          (add-pattern (http:function-patterns function) new-pattern)))

  (:method ((known-patterns cons) (new-pattern http:resource-pattern))
    (let ((patterns-not-subsumed (loop for known-pattern in known-patterns
                                       if (equalp (http:resource-pattern-path known-pattern) (http:resource-pattern-path new-pattern))
                                       do (return-from add-pattern known-patterns)
                                       if (pattern-subsumes-p new-pattern known-pattern)
                                       do (setf (http:resource-pattern-subpatterns new-pattern)
                                                (add-pattern (http:resource-pattern-subpatterns new-pattern) known-pattern))
                                       else collect known-pattern)))
      (loop for known-pattern in patterns-not-subsumed
            if (pattern-subsumes-p known-pattern new-pattern)
            do (progn (setf (http:resource-pattern-subpatterns known-pattern)
                            (add-pattern (http:resource-pattern-subpatterns known-pattern) new-pattern))
                      (return known-patterns))
            finally (return (merge 'list (list new-pattern) patterns-not-subsumed #'<
                                 :key #'pattern-wildcard-count)))))

  (:method ((known-patterns null) (new-pattern http:resource-pattern))
    (list new-pattern)))


(defun ensure-dispatch-function (name &key package
                                      resource-function-class
                                      (generic-function-class 'http:dispatch-function))
  "Define a generic function to use as a server's dispatch mediator for
  request resources. The initial operator includes a method which accepts path
  strings, interns them as resources, and recurses to invoke the resource-specific implementation.
  Should no dispatch method match the path, it signals a not-found condition."

  (let* ((function (ensure-generic-function name
                                            :lambda-list '(resource request response)
                                            :documentation (format nil "the request dispatch function for package '~a'" package)
                                            :generic-function-class generic-function-class))
         (t-class (find-class 't)))
    (setf (http:function-patterns function) nil)
    (when resource-function-class
      (setf (http:function-resource-function-class function) resource-function-class))
    (when package
      (setf (http:function-package function) package))
    ;; add a method for strings which interns them as resources or, failing that signals 
    ;; a not-found condition
    (c2mop:ensure-method function
                         `(lambda (resource-path request response)
                            (let ((http:*resource* (http:bind-resource (function ,name) resource-path request)))
                              (http:log-debug *trace-output* "~a: dispatching ~a -> ~a ~a ~a"
                                            ',name resource-path http:*resource* request response)
                              (cond (http:*resource*
                                     (,name http:*resource* request response))
                                    (t
                                     (http:not-found "No resource defined for path: ~s." resource-path)))))
                         :qualifiers '()
                         :lambda-list '(resource request response)
                         :specializers (list (find-class 'string) t-class t-class))
    ;; locate known operators in the given package and define a dispatch method for each
    (compute-dispatch-methods function)
    function))


(defgeneric compute-dispatch-methods (acceptor)
  (:documentation
    "Given a dispatch function for generic resource functions, locate all
    resource functions from its configured package, extract all acceptable method
    implementation and define respective dispatch methods which augment the call
    arguments with singletons for the path, the media type and the accept media
    types, and invokes the respective implementation method. 
    no longer true:
    In addition, introduce a method to accept and intern the resource path and,
    should that fail because the path did not match the resource class for any
    defined method, to signal a not-found exception.

    In order to construct the dispatch logic, add one method for each resource class
    to the dispatch function to integrate its resource pattern into a
    discrimination net, of which each node of the hierarchy binds a path pattern
    to the respective class.")

  (:method ((acceptor http:acceptor))
    (compute-dispatch-methods (http:acceptor-dispatch-function acceptor)))

  (:method ((dispatch-function http:dispatch-function))
    (let* ((resource-functions (loop for symbol being each external-symbol in (http:function-package dispatch-function)
                                     for function = (when (fboundp symbol) (symbol-function symbol))
                                     when (http:resource-function-p function)
                                     collect function))
           (root-resource-class (http:function-resource-class dispatch-function)))
      (http:log-debug *trace-output* "dispatch function: ~s~%resource-functions: ~a"
                      dispatch-function
                      (mapcar #'c2mop:generic-function-name resource-functions))
      (loop for resource-function in resource-functions
            for name = (c2mop:generic-function-name resource-function)
            do (http:log-debug *trace-output* "adding resource-function: ~s" resource-function)
            do (loop for method in (c2mop:generic-function-methods resource-function)
                     for resource-class = (first (c2mop:method-specializers method))
                     if (and (subtypep resource-class root-resource-class)
                               (http-verb-list-p (method-qualifiers method)))
                     do (progn (http:log-debug *trace-output* "adding resource dispatch method: ~a ~a ~s"
                                               name (class-name resource-class) (method-qualifiers method))
                               (http:define-dispatch-method dispatch-function name resource-class))
                     else do (http:log-debug *trace-output* "skipping method: ~a ~a ~s"
                                          name (class-name resource-class) (method-qualifiers method)))))
    (http:log-debug *trace-output* "Dispatch patterns: ~s"
                    (http:function-patterns dispatch-function))
    dispatch-function))


(defgeneric http:bind-resource (specializer path request)
  (:documentation
   "Given specializer - a resource-class, and path - an url path string,
   perform a depth-first search through the respective class hierarchy to discover the
   first most specialized class for which the resource pattern matches the path.
   GIven a match, return a new resource instance which binds the register values from the
   regular expression match.")

  (:method ((function http:dispatch-function) (path t) request)
    (http:bind-resource (http:function-patterns function) path request))

  (:method ((patterns list) (path string) request)
    (loop with parsed-path = (split-string path #(#\/))
          for pattern in patterns
          for (matched-pattern properties) = (multiple-value-list (match-pattern pattern parsed-path))
          when matched-pattern
          return (apply #'http:make-resource (http:resource-pattern-class matched-pattern) request
                        :path path
                        properties))))


(defgeneric http:make-resource (class request &rest args)
  (:documentation "Provide an interface operator to permit specialized resource construction.
   The base method for symbols resolve the class metaobject and delegates to that.
   The base method for classes delegates to make-instance.")
  (:method ((class symbol) request &rest args)
    (declare (dynamic-extent args))
    (apply #'http:make-resource (find-class class) request args))
  (:method ((class class) (request-arg t) &rest args &key (request request-arg) &allow-other-keys)
    (declare (dynamic-extent args))
    (apply #'make-instance class :request request args)))


;;;
;;;  request

(defmethod print-object ((object http:request) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~a ~a"
            (_slot-value object 'method)
            (ignore-errors (http:request-path object)))))

(defgeneric http:request-acceptor (request)
  )

(defgeneric http:request-auth-token (request)
  (:documentation
    "Given a request, return an authentication token, if present.")
  )

(defgeneric http:request-authentication (request)
  (:documentation
    "Given a request, return as two values, the user name and the password,
    if present.")
  )

(defgeneric http:request-accept-content-encoding (request)
  (:method :around ((request http:request))
    (compute-accept-encoding-ordered-codings (call-next-method))))

(defgeneric http:request-accept-header (request)
  )

(defgeneric http:request-accept-charset (request)
  )

(defgeneric http:request-body (request)
  (:documentation "Return the request body as a single sequence.
    If content-length was supplied, that determines the sequence length.
    If none was provided, the stream must have chunked content-encoding and is
    read to completion.
    In both cases, if the configured request limit is exceeded, a request-entity-too-large
    error is signaled.")
  (:method ((request http:request))
    (let* ((length (or (http:request-content-length request) http:*content-initial-length*))
           (stream (http:request-content-stream request))
           (content (make-array length :element-type (stream-element-type stream) :adjustable t)))
      (http:copy-stream stream content :length (or (http:request-content-length request) http:*content-length-limit*))
      content)))

(defgeneric http:request-cache-matched-p (request etag time)
  (:method ((request http:request) etag time)
    (let ((if-match (http::request-if-match request)))
      (and (or (null if-match)
               (find etag if-match :test #'equalp)
               (find "*" if-match :test #'equalp))
           (loop for request-time in (http:request-if-modified-since request)
             unless (<= time request-time)
             return nil)
           (loop for request-time in (http:request-unmodified-since request)
             when (<= time request-time)
             return nil)
           t))))

(defgeneric http:request-content-stream (request)
  )

(defgeneric http:request-content-length (request)
  )

(defgeneric http:request-content-type-header (request)
  )

(defgeneric http:request-etags (request)
  )

(defgeneric http:request-header (request key)
  (:method ((headers list) key)
    (rest (assoc key headers :test #'string-equal))))

(defgeneric http:request-headers (request)
  )

(defgeneric http:request-is-interactive (request)
  (:method ((request http:request))
    (http:request-is-interactive (http:request-user-agent request)))
  (:method ((user-agent null))
    nil)
  (:method ((user-agent string))
    (loop for (pattern . properties) in http:*user-agent-properties*
      when (cl-ppcre:scan pattern user-agent)
      do (return (getf properties :interactive)))))


(defgeneric (setf http:request-property) (value request key)
  )

(defgeneric http:request-property (request key)
  )

(defgeneric http:request-host (request)
  )

(defgeneric http::request-if-match (request)
  )

(defgeneric http:request-if-modified-since (request)
  )

(defgeneric http:request-keep-alive-p (request)
  )

(defgeneric http:request-media-type (request)
  (:method ((request http:request))
    (if (slot-boundp request 'media-type)
      (request-media-type request)
      (setf (request-media-type request)
            (let ((header (http:request-content-type-header request)))
              (when header
                (mime:mime-type header  :if-does-not-exist 'mime:unsupported-mime-type )))))))

(defgeneric http:request-method (request)
  (:documentation "Upon first reference, cache the effective http verb. This is sought from among the
   various protocl-level and request-specific hiding places with the fall bask to the actual request
   header.")
  (:method ((request http:request))
    (or (get-request-method request)
        (setf-request-method (flet ((as-method-key (string)
                                      (or (find-symbol (string-upcase string) *http-method-package*)
                                          (http:not-implemented :method string))))
                               (let ((header-method (http:request-header request :x-http-method-override)))
                                 (if header-method
                                   (as-method-key header-method)
                                   (if (and (eq (http:request-method request) :post)
                                            (typep (http:request-media-type request) 'mime:application/x-www-form-urlencoded))
                                     (let ((post-method (http:request-post-argument request :_method)))
                                       (if post-method
                                         (as-method-key post-method)
                                         (http:request-original-method request)))))))
                             request))))

(defgeneric http:request-negotiated-character-encoding (request)
  )

(defun negotiate-content-encoding (acceptable-encodings supported-encodings)
  (loop for (encoding . qvalue) in acceptable-encodings
        do (cond ((= qvalue 0)
                  ;; if nothing else matched, cannot satisfy the request
                  (http:not-acceptable))
                 ((or (string-equal encoding :identity) (string-equal encoding "*"))
                  ;; either case, no encoding to do
                  (return nil))
                 ((setf encoding (find encoding supported-encodings :test #'string-equal))
                  (return encoding)))))
;;; (negotiate-content-encoding '((:gzip . 1) (* . 0)) '(:bzip2))
;;; (negotiate-content-encoding '((:gzip . 1)) '(:bzip2))
;;; (negotiate-content-encoding '(("gzip" . 1)) '(:gzip))

(defgeneric http:request-negotiated-content-encoding (request supported-encodings)
  (:method ((request http:request) (supported-encodings list))
    (if (slot-boundp request 'negotiated-content-encoding)
      (get-request-negotiated-content-encoding request)
      (let ((coding (negotiate-content-encoding (http:request-accept-content-encoding request)
                                                supported-encodings)))
        (setf-request-negotiated-content-encoding coding request)))))

(defgeneric http:request-original-method (request)
  )

(defgeneric http:request-path (request)
  )

(defgeneric http:request-post-argument (request key)
  )

(defparameter http::*methods-for-post-parameters* '(:put :post))

(defgeneric http:request-post-argument-list (request &key methods)
  )

(defgeneric http:request-post-arguments (request key)
  )

(defgeneric http:request-query-argument (request key)
  )

(defgeneric http:request-query-arguments (request key)
  )

(defgeneric http:request-argument-list (request)
  (:documentation "Return the consolidated query and post arguments, in that order, as
    an a-list of string pairs, of the form (name . value)"))

(defgeneric http:request-remote-ip-address (request)
  (:documentation
    "Given a request, return the remote ip address"))

(defgeneric http:request-session-id (request)
  (:documentation
    "Given a request, return a session id, if present."))

(defgeneric http:request-unmodified-since (request)
  )

(defgeneric http:request-uri (request)
  )

(defgeneric http:request-uri-host-name (request)
  (:method ((request http:request))
    (let ((uri (http:request-uri request)))
      (when uri
        (puri:uri-host (puri:uri (http:request-uri request)))))))

(defgeneric http:request-user-agent (request)
  (:method ((request http:request))
    (http:request-header request :user-agent)))

;;; resource

(defmethod print-object ((object http:resource) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~a" (when (slot-boundp object 'path) (http:resource-path object)))))

(defgeneric http:resource-request-argument (resource name)
  (:method ((resource http:resource) name)
    (let ((request (http:resource-request resource)))
      (when request
        (or (http:request-query-argument request name)
            (http:request-post-argument request name))))))

(defgeneric http:anonymous-resource-p (resource)
  (:documentation "Return true iff the resource permits some form of access without authentication.
   The applies to cases where the implementation does not establish an agent or the agent itself
   associates no identity.
   The default method returns false.")
  (:method ((resource http:resource))
    nil))

(defgeneric http:resource-file-type (resource)
  (:method ((path string))
    (let ((dot-position (position #\. path :from-end t)))
      (when dot-position (subseq path (1+ dot-position)))))
  (:method ((path t))
    nil)
  (:method ((resource http:resource))
    (http:resource-file-type (http:resource-path resource))))

;;;
;;; def-resource

#+(or)
(defmacro http:def-resource ((name pattern &rest keywords) parent-classes slots &rest options)
  (unless (assoc :metaclass options)
    (push '(:metaclass http:resource-class) options))
  (unless (assoc :direct-superpatterns options)
    (push `(:direct-superpatterns ,@parent-classes) options))
  `(defclass ,name ,(or parent-classes '(http:resource))
     ,slots
     (:pattern . ,pattern)
     (:keywords ,@keywords)
     ,@options))

#+digitool
(setf (ccl:assq 'http:def-resource ccl:*fred-special-indent-alist*) 2)

;;;
;;; resource-function method combination

(defmethod add-method :after ((function http:resource-function) method)
  "As a side effect of adding a method, accumulate the method key for documentation."
  (let ((http-qualifiers (intersection http:+method-keys+ (method-qualifiers method))))
    (when http-qualifiers
      (setf (http:function-method-keys function)
            (union (http:function-method-keys function) http-qualifiers)))))

#+(or)
(define-method-combination http:http (&key )
                           ((identification (:auth :identification))
                            (permission (:auth :permission))
                            (around (:around) )
                            (decode (:decode . *))
                            (encode (:encode . *))
                            (primary http-verb-list-p))
  (:generic-function function)
  (flet ((qualify-methods (methods)
           (let ((categorized ())
                 (function-method-keys (http:function-method-keys function)))
             (loop for method in methods
                   do (loop for method-key in (method-qualifiers method)
                            do (case method-key
                                 ((:decode :encode) )
                                 (* (loop for method-key in function-method-keys
                                          do (push method (getf categorized method-key))))
                                 (t (push method (getf categorized method-key))))))
             (loop for (key methods) on categorized by #'cddr
                   collect key collect (reverse methods)))))
    (print (list :identification identification
                 :permission permission
                 :around around
                 :decode decode
                 :encode encode
                 :primary primary))
    (if (and encode (eq (second (method-qualifiers (first encode))) :as))
      `(call-method ,(first encode) ())
      (let* ((form
              `(handler-case
                 ,(compute-effective-resource-function-method function
                                                              identification permission
                                                              around 
                                                              decode
                                                              (qualify-methods primary)
                                                              encode)
                 (redirection-condition (redirection)
                                         ;; if the redirection is internal invoke it, otherwise resignal it
                                         (let ((location (http:condition-location redirection)))
                                           (if (functionp location)
                                             (funcall location)
                                             (error redirection)))))))
        form))))

(defparameter *define-method-combination.verbose* nil)

(define-method-combination http:http (&key)
                           (;; must be distinct as the methods can have the same signature
                            (authenticate-password (:auth http:authenticate-request-password))
                            (authenticate-token (:auth http:authenticate-request-token))
                            (authenticate-session (:auth http:authenticate-request-session))
                            (authenticate-location (:auth http:authenticate-request-location))
                            (authorize-request (:auth http:authorize-request))
                            (auth-around (:auth :around))
                            (around (:around) )
                            (decode (:decode . *)) ;; should be singleton
                            (encode (:encode . *))
                            (get (:get))        ; nb. cannot be required as the :as definitions
                            (put (:put))        ; cut processing off to delegate to anther media type
                            (head (:head))
                            (patch (:patch))
                            (post (:post))
                            (delete (:delete))
                            (options (:options))
                            (log (:log))        ; internal log
                            (trace (:trace))    ; http trace
                            (connect (:connect))
                            (multiple http-verb-list-p))
  (:generic-function function)
  (flet ((qualify-methods ()
           (let ((categorized (append (when get `(:get ,(reverse get)))
                                      (when put `(:put ,(reverse put)))
                                      (when head `(:head ,(reverse head)))
                                      (when patch `(:patch ,(reverse patch)))
                                      (when post `(:post ,(reverse post)))
                                      (when delete `(:delete ,(reverse delete)))
                                      (when options `(:options ,(reverse options)))
                                      (when trace `(:trace ,(reverse trace)))
                                      (when connect `(:connect ,(reverse connect))))))
             (loop for method in multiple
                   do (loop for method-key in (method-qualifiers method)
                            do (push method (getf categorized method-key))))
             (loop for (key methods) on categorized by #'cddr
                   collect key collect (reverse methods)))))
    (let* ((verb-methods (qualify-methods))
           (form (cond ((and encode (eq (second (method-qualifiers (first encode))) :as))
                       `(call-method ,(first encode) ()))
                      ((and decode (eq (second (method-qualifiers (first decode))) :as))
                       `(call-method ,(first decode) ()))
                      (t
                       (let* ((form
                               `(handler-case
                                  ,(compute-effective-resource-function-method function
                                                                               (append authenticate-password authenticate-token
                                                                                       authenticate-session authenticate-location)
                                                                               authorize-request
                                                                               auth-around
                                                                               around 
                                                                               decode verb-methods encode)
                                  (redirection-condition (redirection)
                                                         ;; if the redirection is internal invoke it, otherwise resignal it
                                                         (let ((location (http:condition-location redirection)))
                                                           (if (functionp location)
                                                             (funcall location)
                                                             (error redirection)))))))
                         form)))))
      (when log
        (setf form `(progn (call-method ,(first log) ()) ,form)))
      form)))


(defgeneric log-http-function (function-name resource request response content-type accept-type methods)
  (:method :around ((function t) resource request response content-type accept-type methods)
           (http.i::call-if-log-level-qualifies :trace #'call-next-method))
  (:method ((function t) resource request response content-type accept-type methods)
    (let* ((*print-pretty* nil)
           (arguments (list resource request response content-type accept-type)))
      (format *trace-output* "~%function:  ~a: verb: ~a" function (http:request-method request))
      (format *trace-output* "~%headers:   ~s" (http:request-headers request))
      (format *trace-output* "~%arguments: ~s"  arguments)
      (format *trace-output* "~%methods:   ~{~s~^~%~11t~}" methods)))
  (:method :after ((function-name t) (resource t) (request t) (response t) (content-type t) (accept-type t) (methods t))
           (terpri *trace-output*)
           (finish-output *trace-output*)))


(defgeneric http:respond-to-option-request (function request response verbs)
  (:documentation "The base method implements the default heade response.
   It collected the verbs from the applicable methods into the 'Allow' header and emits
   the headers.")
  (:method ((function t) (request t) (response http:response) verbs)
    ;; the default method 
    (setf (http:response-content-length response) 0)
    (setf (http:response-allow response) verbs)
    nil))


;; the sbcl code for make method fails to allow for a method which does nothing
;; and always warns about unused parameters, instead of (make-method nil), use a real method

(defgeneric the-null-function (resource request response content-type accept-type)
  (:method ((resource t) (request t) (response t) (content-type t) (accept-type t))
    nil))
(defparameter *the-null-method* (first (c2mop:generic-function-methods #'the-null-function)))


(defgeneric the-unsupported-function (resource request response content-type accept-type)
  (:method ((resource t) (request http:request) (response t) (content-type t) (accept-type t))
    (http:unsupported-media-type "Media type not supported: ~s." (http:request-media-type request))))
(defparameter *the-unsupported-method* (first (c2mop:generic-function-methods #'the-unsupported-function)))

(defun compute-effective-resource-function-method (function authentication authorization authentication-around
                                                            around
                                                            decode-methods primary-by-method encode-methods)
  "arrange the response methods to effect authentication, generate content, and encode it.
  in addition interpose operations to
  - configure the output stream based on the requested media type
  - ensure that the headers are sent
  
  1. execute authentication methods (unordered) as an (or ...) and require a true result.
     if not, signal not-authorized.
  1.a given methods marked authentication and around, they wrap averything
  2. around wraps everything _post_ authentication
  3. compute the response media type from the most specific encoding method specializer
     and the requested character encoding and cache it in the response stream
  ;; 4--7 cannot happen here as, eg when the encoding method is to emit a string, it should set
  ;; the content length header, which must be emitted and would suppress chunking
  ;; 4. if a content encoding is required, interpose a zip stage (nyi)
  ;; 5. execute the combined content and decoding methods
  ;; 6. configure the response stream
  ;; 7. ensure that the headers are emitted
  5. apply the most specific encoding auxiliary to the result content methods - use jus the most speific in
     order that is next method is always content
  6. use all decoding method to produce the content methods' input in order that they can pipe content"

  (let* ((authentication-clause (when (or authentication authorization)
                                  ;; _iff_ some auth form is specified,
                                  ;; require that either the agent is already authenticated - eg from redirection
                                  ;; or that one of the identification methods succeed, and that all of the
                                  ;; permission methods succeed
                                  `(unless (and (let ((resource (http:resource))
                                                      (request (http:request)))
                                                  (or (http:request-agent request)
                                                    ,@(loop for method in authentication
                                                            collect `(call-method ,method ()))
                                                    (http:authenticate-anonymous resource request)))
                                                ,@(loop for method in authorization
                                                        collect `(call-method ,method ())))
                                     (http:unauthorized))))
         ;; the most-specific only
         (encode-method (first encode-methods))
         ;; add a clause to cache the concrete media type according to the most
         ;; specific class specializer applicable to the request's accept type
         ;; carrying over the character encoding from the request
         #+(or)
         (mime-type-clause (when encode-methods
                             (let* ((method (first encode-methods))
                                    (specializer (fifth (c2mop:method-specializers method))))
                               `(setf (http:response-media-type (http:response))
                                      (http:response-compute-media-type (http:request) (http:response) ,(class-name specializer)
                                                                        :charset (or (mime:mime-type-charset (http:request-accept-type (http:request))) :utf-8))))))
         ;; build a case statement with one entry for each http operation for which
         ;; known a method is present
         (content-clause `(case (http:request-method http:*request*)
                              ;; add a clause for each verb asspciated with an applicable method
                              ,@(loop for (key methods) on primary-by-method by #'cddr
                                      collect `(,key
                                                ,(if methods
                                                   `(call-method ,(first methods)
                                                                 ,(append (rest methods)
                                                                          (if (member key '(:patch :post :put))
                                                                            ;; include decode methods iff the verb supports content
                                                                            ;; otherwise arrange to return nil
                                                                            (or decode-methods (list *the-unsupported-method*))
                                                                            ;;'((make-method nil))
                                                                            (list *the-null-method*)
                                                                            )))
                                                   '(http:not-implemented "media type (~s) not implemented for method (~s)"
                                                                          (http:request-accept-type http:*request*)
                                                                          (http:request-method http:*request*)))))
                              ;; add an options clause if none is present
                              ,@(unless (getf primary-by-method :options)
                                  `((:options (http:respond-to-option-request ,function (http:request) (http:response)
                                                                              '(:options ,@(loop for (key nil) on primary-by-method by #'cddr collect key))))))
                              ;; otherwise, it is not implemented
                              (t (http:not-implemented "media type (~s) not implemented for method (~s)"
                                                       (http:request-accept-type http:*request*)
                                                       (http:request-method http:*request*)))))
         ;; wrap the decoding an content generation steps with a mechanism to encode the result.
         ;; if the content is null, no output should be generated
         ;; if no method was applicable, generate logic to either derive an alternative concrete media type
         ;; signal a nont-applicable error if that fails.
         (main-clause (if encode-method
                        `(call-method ,encode-method ,(append (rest encode-methods) `((make-method ,content-clause))))
                        content-clause)))
    #+(or)(when mime-type-clause
      (setf main-clause `(progn ,mime-type-clause ,main-clause)))
    ;; run 'plain' around methods within the authenticated context
    (when around
      (setf main-clause `(call-method ,(first around) (,@(rest around) (make-method ,main-clause)))))
    ;; arrange the authentication clause to combine the implicit methods and any explicit around method with
    ;; the main clause
    (if authentication-clause
      (if authentication-around
        (setf main-clause `(progn (call-method ,authentication-around ((make-method ,authentication-clause)))
                                  ,main-clause))
        (setf main-clause `(progn ,authentication-clause ,main-clause)))
      (when authentication-around
        (setf main-clause `(progn (call-method ,authentication-around ())
                                  ,main-clause))))
    ;; ensure the headers are sent
    `(progn
       ,main-clause
       (http:send-headers (http:response)))))

#|
      ;; if there was an encode clause, use the comuted form.
      ;; otherwise, if a response is required, if there was an acceptable concrete content type,
      ;; delegate to its implementation and, if not, signal not-acceptable.
      ;; if no response is required, then the form w/o encoding suffices.
      (if encode-method
        form
        `(if (or (eq :get (http:request-method http:*request*))
                 (http:request-accept-header http:*request*))
           (let ((acceptable-media-type (compute-acceptable-media-type ,function
                                                                       (http:resource) (http:request) (http:response)
                                                                       (http:request-media-type (http:request))
                                                                       (http:request-accept-type (http:request)))))
             (cond (acceptable-media-type
                    (funcall ,function (http:resource) (http:request) (http:response)
                             (http:request-media-type (http:request))
                             acceptable-media-type))
                   (t
                    (setf (http:response-media-type (http:response)) mime:text/plain)
                    (http::not-acceptable))))
           ,form)))))
|#


(defmacro http:def-resource-function (name lambda-list &rest clauses)
  (let* ((method-combination nil)
         (generic-function-class nil)
         (default-encode-media-types ())
         (definition-clauses (loop for clause in clauses
                                   for key = (first clause)
                                   collect (ecase key
                                             (:around
                                              `(:method ,@clause))
                                             ((:documentation declare :method-class  :method)
                                              clause)
                                             (:generic-function-class
                                              (setf generic-function-class (second clause))
                                              clause)
                                             (:method-combination
                                              (setf method-combination (second clause))
                                              clause)
                                             ((:get :put :head :patch :post :delete :options :trace :connect)
                                              (let* ((from-lambda (member-if #'consp clause))
                                                     (qualifiers (ldiff clause from-lambda)))
                                                (destructuring-bind (lambda-list &body body) from-lambda
                                                  `(:method ,@qualifiers
                                                     ,(if (eql 5 (length lambda-list))
                                                        lambda-list
                                                        (append lambda-list `((,(gensym "content-type") t) (,(gensym "accept-type") t))))
                                                     ,@body))))
                                             (:encode
                                              (case (second clause)
                                                (:default ;; record the default media type
                                                    (setf clause (cons (first clause) (cddr clause)))
                                                    (push (if (consp (second clause))
                                                              (second (fifth (second clause)))
                                                              (second clause))
                                                          default-encode-media-types)))
                                              (if (consp (second clause))
                                                ;; literal definition
                                                `(:method ,@clause)
                                                (let ((qualifiers (remove-if-not #'keywordp clause))
                                                      (media-types (remove-if #'keywordp clause)))
                                                  (ecase (second qualifiers)
                                                    (:as
                                                     (assert (not (typep (second media-types) (type-of (first media-types)))) ()
                                                             "media type delegation is circular: ~s > ~s"
                                                             (first media-types) (second media-types))
                                                     `(:method ,@qualifiers ((resource t) (request t) (response t) (content-type t) (accept-type ,(first media-types)))
                                                        ;; replace the currently noted type the second one
                                                        (http:log-trace *trace-output* "redirecting ~a: ~a -> ~a" ',name 
                                                                        ,(first media-types) ,(second media-types))
                                                        (setf (http:response-media-type response) ,(second media-types))
                                                        (,name resource request response content-type ,(second media-types))
                                                        ;; have the generated method rrturn nil, to terminate encoding
                                                        nil))
                                                    ((nil)
                                                     `(:method ,@qualifiers ((resource t) (request t) (response t) (content-type t) (accept-type ,(first media-types)))
                                                        ;; encode as per the derived response content type, which will be an instance of the
                                                        ;; specializer class, but include the character set encoding
                                                        (let ((content (call-next-method))
                                                              (effective-content-type (http:response-media-type response)))
                                                          (http:log-trace *trace-output* "content-type: ~a: content ~s"
                                                                          effective-content-type content)
                                                          (http:encode-response content response effective-content-type)
                                                          ;; have the generated method rrturn nil, to terminate encoding
                                                          nil)))))))
                                             (:decode clause
                                                      (if (consp (second clause))
                                                        ;; literal definition
                                                        `(:method ,@clause)
                                                        (let ((qualifiers (remove-if-not #'keywordp clause))
                                                              (media-types (remove-if #'keywordp clause)))
                                                          (ecase (second qualifiers)
                                                            (:as
                                                             `(:method ,@qualifiers ((resource t) (request t) (response t) (content-type ,(first media-types)) (accept-type t))
                                                                       (,name resource request response ,(second media-types) accept-type)))
                                                            ((nil)
                                                             `(:method ,@qualifiers ((resource t) (request t) (response t) (content-type ,(first media-types)) (accept-type t))
                                                                       (http:decode-request resource request content-type)))))))
                                             (:log
                                              (if (rest clause)
                                                `(:method ,@clause)
                                                `(:method :log ((resource t) (request t) (response t) (content-type t) (accept-type t))
                                                          (http:log-debug *trace-output* "~s: ~s ~s ~s ~s ~s"
                                                                          ',name resource request response content-type accept-type))))
                                             (:auth
                                              (if (third clause)
                                                `(:method ,@clause)
                                                `(:method ,@(subseq clause 0 2) ((resource t) (request t) (response t) (content-type t) (accept-type t))
                                                          (,(second clause) resource request))))))))
    (unless method-combination
      (push '(:method-combination http:http) definition-clauses))
    (if generic-function-class
      (assert (subtypep generic-function-class 'http:resource-function))
      (push '(:generic-function-class http:resource-function) definition-clauses))
    (let ((lambda-list (ecase (length lambda-list)
                         (0 '(resource request response content-type accept-type))
                         (1 (append lambda-list '(request response content-type accept-type)))
                         (3 (append lambda-list '(content-type accept-type)))
                         (5 lambda-list))))
      ;; there is an issue with tracing these operators:
      ;; the binding process precludes a tracing wrapper, which means that must happen after the functions
      ;; are bound to the acceptor
      `(progn
         (defgeneric ,name ,lambda-list
           (:argument-precedence-order ,(first lambda-list) ,@(subseq lambda-list 3 5) ,@(subseq lambda-list 1 3))
           ,@definition-clauses)
         ,@(when default-encode-media-types
             `((setf (http:function-default-accept-header (function ,name))
                     ,(format nil "~{~a~^,~}" default-encode-media-types))))
         (function ,name)))))


(defgeneric funcall-resource-function (function resource request response content-type accept-type)
  (:method ((function symbol) (resource t) (request t) (response t) (content-type t) (accept-header t))
    (funcall-resource-function (cond ((fboundp function) (symbol-function function))
                                     (t (error "undefined resource-function: ~s." function)))
                               resource request response content-type accept-header))

  ;; w/o a response type, just call
  (:method ((function http:resource-function) (resource t) (request http:request) (response t) (content-type t) (accept-header null))
    "if no header was given, use the default"
  ;; (setf (http:request-accept-type request) nil)
  ;; (setf (http:response-media-type response) nil)
  ;; (funcall function resource request response content-type nil)
    (funcall-resource-function function resource request response content-type
                               (http:function-default-accept-header function)))

  ;; specialize on resource-function as its fields are required to compute the accept header and thereby response effective method
  (:method ((function http:resource-function) (resource t) (request http:request) (response t) (content-type t) (accept-header t))
    "call the function with its computed acceptable response content type"
    (let ((media-type (if accept-header
                          (or (resource-function-acceptable-media-type function accept-header)
                              (let ((default (http:function-default-accept-header function)))
                                 (when (and (equal accept-header "*/*") (not (equal default "*/*")))
                                   (resource-function-acceptable-media-type function default)))
                              (http::not-acceptable "Media type (~s) not implemented." accept-header))
                          ;;absent an exceptable type, try the function's default.
                          (resource-function-acceptable-media-type function
                                                                   (http:function-default-accept-header function)))))
      (cond (media-type
             (setf (http:request-accept-type request) media-type)
             (setf (http:response-media-type response)
                   (http:response-compute-media-type request response media-type
                                                     :charset (or (mime:mime-type-charset media-type) :utf-8)))
             (funcall function resource request response content-type media-type))
            ((member (http:request-method request) '(:patch :put :post :delete))
             ;; if the method expects no response, use test/plain as place holder
             (setf (http:request-accept-type request)
                   (setf (http:response-media-type response) mime:text/plain))
             (funcall function resource request response content-type mime:text/plain))
            (t
             (http::not-acceptable "Media type (~s) not implemented." accept-header))))))


(:documentation "media type computation"
 "GIven a resource function which implements some set of response encodings and the weighted accept header
 from a request, compute the composite media type argument to supply in the function invocation.

 - resource-function-acceptable-media-type 
   - compute-accept-ordered-types : parse the accept header  and order the media types by weight
   - resource-function-acceptable-media-types : limit the accept list to tose which are subtypes of some implemented type and produce a composite
   - resource-function-accept-types : manage a cache from accept header to composite type 

 the compoiste type is then supplied to the function to select the applicable encoding methods in the order specified by the accept header"
 )

(defgeneric resource-function-media-types (function)
  (:documentation "Return the set of media type specializers for which encode methods are defined in the function.
    This is without regard to other specializers and is used to compute from a request accept media type specification
    the proxy instance which serves as the response media type argument to call the function.")
  (:method ((function http:resource-function))
    (remove-duplicates (loop for method in (c2mop:generic-function-methods function)
                             for specializer = (fifth (c2mop:method-specializers method))
                             when (and (eq :encode (first (method-qualifiers method)))
                                       (subtypep specializer 'mime:mime-type))
                             collect (class-name specializer)))))

#+(or) ; supercede type construction approach based on type to diret selectio based on instances
(defgeneric resource-function-acceptable-media-types (function accept-types)
  (:documentation "Return the ordered sequence of those of the functions encoding media type specializers
   which are subtypes of the given list of accept types. The order is respective the accept types and any
   duplicates are removed from the end.")
  (:method ((function http:resource-function) (accept-types list))
    (sort (remove-duplicates (loop with defined-types = (resource-function-media-types function)
                                   for accept-type in accept-types
                                   append (loop for defined-type in defined-types
                                                when (eq accept-type defined-type)
                                                ;; a subtype constraint is possible, but can also introduce unintended matches
                                                ;; when (or (eq accept-type defined-type) (subtypep accept-type defined-type))
                                                collect defined-type))
                             :from-end t)
          #'subtypep)))

(defgeneric resource-function-acceptable-media-types (function accept-types)
  (:documentation "Return the ordered sequence of those of the functions encoding media type specializers
   which are subtypes of the given list of accept types. The order is respective the accept types and any
   duplicates are removed from the end.")
  (:method ((function http:resource-function) (accept-types list))
    (loop with defined-types = (resource-function-media-types function)
      for accept-type in accept-types
      when (some #'(lambda (defined) (typep accept-type defined)) defined-types)
      collect accept-type)))


(defgeneric resource-function-acceptable-media-type (function candidate-types)
  (:documentation "Combine the media types from the request accept header with those defined for the function
    to derive a composite type compatible with the function's methods. If none are compatible, return nil.")
  
  (:method ((function http:resource-function) (accept-media-type mime:mime-type))
    "Given a mime type instance, return it."
    accept-media-type)

  (:method ((function http:resource-function) (accept-header string))
    "Given an accept header string, canonicalize it, extract tne media type set, compute the compound type
     for those, and cache it in the function, and return it"
    (setf accept-header (remove #\space accept-header))
    (let ((cache (resource-function-accept-types function)))
      (or (rest (assoc accept-header cache :test #'string-equal))
          (let ((type (resource-function-acceptable-media-type function (compute-accept-ordered-types accept-header))))
            (setf (resource-function-accept-types function)
                  (acons accept-header type cache))
            type))))
  
  (:method ((function http:resource-function) (accept-specification cons))
    "Given a media type set, generate a composite media type from the match against the encode function's
     methods."
    (unless (every #'mime:mime-type-p accept-specification)
      (http:bad-request "Invalid accept specification: ~s." accept-specification))
    (let* ((acceptable-type-list (resource-function-acceptable-media-types function accept-specification)))
      (first acceptable-type-list))))

#|
obsolete mechanism which was in terms of the encode methods

(defgeneric compute-acceptable-methods (function resource request response request-type response-type)
  (:documentation "compute applicable methods, but 
     - select from encoding method only
     - constrain the function to be a resource function
     - invert the type relation for the response type")

  (:method ((function http:resource-function) resource request response request-type (response-type mime:mime-type))
    (compute-acceptable-methods function resource request response request-type (class-of response-type)))

  (:method ((function http:resource-function) resource request response request-type (response-type-class class))
    (loop for method in (c2mop:generic-function-methods function)
          when (and (eq :encode (first (method-qualifiers method)))
                    (destructuring-bind (resource-q request-q response-q request-type-q response-type-q)
                                        (c2mop:method-specializers method)
                      (and (typep resource resource-q)
                           (typep request request-q)
                           (typep response response-q)
                           (typep request-type request-type-q)
                           (subtypep (class-name response-type-q) response-type-class))))
          collect method)))

(defgeneric compute-acceptable-media-type (function resource request response request-type response-type)
  (:method ((function http:resource-function) resource request response request-type (response-type mime:mime-type))
    (let ((methods (compute-acceptable-methods function resource request response request-type response-type)))
      (when methods
        (make-instance (fifth (c2mop:method-specializers (first methods))))))))
|#

;;;
;;; resource patterns modeled explicitly

(defun parse-resource-name-pattern (name)
  (loop for element in (split-string name #(#\/))
    when (plusp (length element))
    collect (if (char= *keyword-marker-character* (char element 0))
                (if (char= #\* (char element (1- (length element))))
                    (list (cons-symbol :keyword (subseq element 1 (1- (length element)))))
                    (cons-symbol :keyword (subseq element 1)))
                element)))
;;; (parse-resource-name-pattern "/:account/:repository")
;;; (parse-resource-name-pattern "/:account/:repository/:detail*")

(defmethod initialize-instance ((instance http:resource-pattern) &rest initargs &key
                                class
                                (name (class-resource-path class)))
  (declare (dynamic-extent initargs))
  (let ((path (parse-resource-name-pattern name)))
    (flet ((test-path (test-path)
             (loop (let ((pattern-element (pop path))
                         (test-element (pop test-path)))
                     (if pattern-element
                       (if test-element
                         (unless (or (string-equal pattern-element "*")
                                     (string-equal pattern-element test-element))
                           (return nil)))
                       (return (null test-element)))))))
      (setf (slot-value instance 'path) path)
      (setf (slot-value instance 'predicate) #'test-path))
    (apply #'call-next-method instance
           :name name
           initargs)))

(defgeneric class-resource-path (class)
  (:method ((class class)) (symbol-name (class-name class)))
  (:method ((name symbol)) (symbol-name name)))

(defgeneric pattern-wildcard-count (pattern)
  (:method ((pattern http:resource-pattern))
    (count-if #'keywordp (http:resource-pattern-path pattern))))
          
(defmethod print-object ((instance http:resource-pattern) stream)
  (print-unreadable-object (instance stream :identity nil :type t)
    (format stream "? ~a~@[ ~a~]" (http:resource-pattern-name instance) (http:resource-pattern-subpatterns instance))))

;; (make-instance 'http:resource-pattern :name "*/account/*" :class t)


(defgeneric pattern-subsumes-p (p1 p2)
  (:method ((p1 http:resource-pattern) (p2 http:resource-pattern))
    (pattern-subsumes-p (http:resource-pattern-path p1) (http:resource-pattern-path p2)))
  (:method ((p1 null) (p2 null))
    t)
  (:method ((p1 null) (p2 cons))
    t)
  (:method ((p1 cons) (p2 null))
    nil)
  (:method ((p1 cons) (p2 cons))
    (when (or (equalp (first p1) (first p2)) (keywordp (first p1)))
      (pattern-subsumes-p (rest p1) (rest p2)))))

#+(or)
(defun merge-patterns (p1 p2)
  (cond ((equalp (http:resource-pattern-path p1) (http:resource-pattern-path p2))
         p1)
        ((pattern-subsumes-p p1 p2)
         (setf (http:resource-pattern-subpatterns p1)
               (add-pattern (http:resource-pattern-subpatterns p1) p2))
         p1)
        ((pattern-subsumes-p p2 p1)
         (setf (http:resource-pattern-subpatterns p2)
               (add-pattern (http:resource-pattern-subpatterns p2) p1))
         p2)))

(defgeneric match-pattern (pattern path)
  (:documentation
    "Given a pattern instance or path, perform a trival match to a given concrete path.
     This proceeds by element, where keyword pattern elements match any concrete path element and collect
     the keyword and concrete value into a property list. String pattern element match as equal to concrete
     element, but add nothing to the result properties. If all elements match, return as values the pattern
    instance and the property list.")

  (:method ((pattern t) (path string))
    (match-pattern pattern (split-string path #(#\/))))

  (:method ((pattern http:resource-pattern) (path list))
    (flet ((try-subpattern ()
             (loop for subpattern in (http:resource-pattern-subpatterns pattern)
                   for (sub-class sub-properties) = (multiple-value-list (match-pattern subpattern path))
                   when sub-class
                   do (return-from match-pattern (values sub-class sub-properties)))))
      (multiple-value-bind (match-p properties) (match-pattern (http:resource-pattern-path pattern) path)
        (case match-p
          ((nil) nil)
          (:complete
           (try-subpattern)
           (values pattern properties))
          (:partial
           (try-subpattern))))))

  (:method ((pattern null) (path null))
    (values :complete nil))
  (:method ((pattern null) (path cons))
    (values :partial nil))
  (:method ((pattern cons) (parsed-path list))
    (etypecase (first pattern)
      (string (when (equal (first pattern) (first parsed-path))
                (match-pattern (rest pattern) (rest parsed-path))))
      (keyword (when parsed-path
                 (multiple-value-bind (match-p properties) (match-pattern (rest pattern) (rest parsed-path))
                   (when match-p
                     (values match-p (list* (first pattern) (first parsed-path) properties))))))
      (http:resource-pattern (loop for pattern in pattern
                                   for (matched-pattern properties) = (multiple-value-list (match-pattern pattern parsed-path))
                                   when matched-pattern
                                   return (values matched-pattern properties)))
      (cons (destructuring-bind (term) (first pattern)
              (values :complete (list term parsed-path)))))))
;;;
;;; response

(defgeneric (setf http:response-header) (value response header-label)
  )

(defgeneric http:response-header (response header-label)
  )

(defgeneric (setf http:response-accept-encoding) (accept-codings response)
  (:method ((values list) (response http:response))
    (setf (http:response-accept-encoding response) (format nil "~(~{~a~^,~}~)" values))))

(defgeneric (setf http:response-accept-ranges) (ranges response)
  (:method ((value null) (response http:response))
    (setf (http:response-accept-ranges response) "none")))

(defgeneric (setf http:response-allow) (allow-verbs response)
  (:method ((allow-verbs list) (response http:response))
    (setf (http:response-allow-header response) (format nil "~{~a~^,~}" allow-verbs))))

(defgeneric (setf http:response-allow-header) (allow-verbs response)
  )

(defgeneric (setf http:response-cache-control) (value response)
  )

(defgeneric (setf http:response-character-encoding) (character-encoding response)
  )

(defgeneric http:response-headers (response)
  )

(defgeneric http:response-compute-media-type (request response class &key charset)
  (:method ((request t) (response http:response) (type mime:mime-type) &rest args)
    "make an argument-specific version if arguments, eg charset, is supplied"
    (declare (dynamic-extent args))
    (if args
        (apply #'mime:mime-type type args)
        type)))

(defgeneric (setf http:response-content-disposition) (disposition response)
  )

(defgeneric (setf http:response-content-encoding) (content-coding response)
  (:method ((coding symbol) (response http:response))
    (setf (http:response-content-encoding response) (string-downcase coding))))

(defgeneric (setf http:response-content-length) (value response)
  ;; hunchentoot requires an integer argument
  (:method ((value integer) (response http:response)) 
    (setf (http:response-content-length-header response) value)))

(defgeneric (setf http:response-content-length-header) (value response)
  )

(defgeneric http:response-content-stream (response)
  (:documentation "Return the response content stream while ensuring that
   the response head has been sent before the body")
  (:method ((response http:response))
    ;; ensure the headers are staged
    (unless (http:response-headers-sent-p response)
      (http:send-headers response))
    (get-response-content-stream response)))

(defgeneric (setf http:response-content-type-header) (content-type-header response)
  )

(defgeneric (setf http:response-etag) (tag response)
  )

(defgeneric (setf http:response-date) (timestamp response)
  (:method ((timestamp integer) (response http:response))
    (setf (http:response-date response) (http:encode-rfc1123 timestamp))))

(defgeneric http:response-headers-sent-p (response)
  (:method ((response http:response))
    (let* ((response-stream (get-response-content-stream response))
           (header-stream (http:stream-header-stream response-stream)))
      (or (not (open-stream-p header-stream))
          (null (stream-file-position header-stream))
          (plusp (stream-file-position header-stream))))))

(defgeneric http:response-keep-alive-p (response)
  (:method ((response http:response))
    (let ((request (http:response-request response)))
      (when request (http:request-keep-alive-p request)))))

(defgeneric (setf http:response-last-modified) (timestamp response)
  (:method ((timestamp integer) (response http:response))
    (setf (http:response-last-modified response) (http:encode-rfc1123 timestamp))))

(defgeneric (setf http:response-location) (location response)
  )

(defgeneric (setf http:response-media-type) (media-type response)
  (:method ((media-type cons) (response http:response))
    (setf (http:response-media-type response)
          (or (mime:mime-type media-type)
              (error "invalid media type: ~s" media-type))))

  (:method ((media-type mime:mime-type) (response http:response))
    (setf (http:response-content-type-header response)
          (mime:mime-type-namestring media-type))
    (setf-response-media-type media-type response)
    media-type))

(defgeneric (setf http:response-retry-after-header) (time response)
  )

(defgeneric http:response-transfer-encoding-header (response)
  )

(defgeneric (setf http:response-transfer-encoding-header) (time response)
  )

(defgeneric (setf http:response-vary) (value response)
  )

(defgeneric (setf http:response-www-authenticate-header) (authentication-method response)
  )




(defgeneric http:report-condition-headers (condition response)
  (:documentation "Given a condition, assert its side-effects on the request
    state, to be communicated to the client as part of the response.")

  (:method :before ((condition t) response)
    ;; no length (setf (http:response-content-length response) 1024)
    ;; ensure text plain for error response
    (setf (http:response-media-type response) mime:text/plain))

  (:method ((condition http:condition) (response t))
    (setf (http:response-status-code response) (http:condition-code condition)))

  (:method ((condition redirection-condition) response)
    (setf (http:response-location response) (http:condition-location condition))
    (call-next-method))

  (:method ((condition http:not-modified) response)
    (setf (http:response-etag response) (http:condition-etag condition))
    (setf (http:response-date response) (http:condition-mtime condition))
    (call-next-method))

  (:method ((condition http:unauthorized) response)
    (setf (http:response-www-authenticate-header response) "Basic")
    (call-next-method))

  (:method ((condition http:internal-error) response)
    (let ((time (http:condition-retry-after condition)))
      (when time
        (setf (http:response-retry-after-header response) time)))
    (call-next-method)))


(defgeneric http:report-condition-body (condition response)
  (:documentation "Given a condition, write it to the response
    content stream. This is intended to be invoked in the error handler
    for request processing once the headers have been sent to report
    error detainls as the response body.")
  (:method ((condition http:condition) (response t))
    ;; (format *trace-output*  "~%~a~%~a~%" (get-response-content-stream response) condition)
    (format (get-response-content-stream response) "~%~a~%" condition)))


(defgeneric http:clear-headers (response)
  (:documentation "Reset any buffered headers which are pending output. This will
    occur when an error occures during response processing, yet the headers have not
    yet been written.")
  (:method ((response http:response))
    (setf (response-state response) nil)
    (stream-clear-header-output (get-response-content-stream response))))


(defgeneric http:send-condition (response condition)
  (:documentation "Attempt to reset the response state and respond with the
   status and report for the given condition.")
  (:method ((response http:response) condition)
    ;; when the headers are still pending, stage an error report as the new headers
    ;; and write the condition report as the content.
    ;; if the headers are already gone, just append to the content and terminate the processing
    (when (http:clear-headers response)
      (http:report-condition-headers condition response)
      (http:send-headers response)
      #+(or)(let ((header-stream (http:stream-header-stream (http:send-headers response))))
        (print :after-send-headers2)
        (dotimes (x 4096) (write-char #\+ header-stream))
        (finish-output header-stream)))
    ;; emit any body
    (http:report-condition-body condition response)
    (finish-output (get-response-content-stream response))))

                               
(defgeneric http:send-entity-body (response body)
  (:documentation
    "Send a monolithic response body.
    Ensure that the headers have been sent, by using the guarded accessor.")

  (:method ((response http:response) (body vector))
    ;; try to disable chunking; for binary sequences length is the byte count
    (setf (http:response-content-length response) (length body))
    (write-sequence body (http:response-content-stream response)))

  (:method ((response http:response) (body string))
    ;; try to disable chunking; for strings, the encoded length is the byte count
    (setf (http:response-content-length response)
          (mime:size-string body (mime:mime-type-charset (http:response-media-type response))))
    (write-string body (http:response-content-stream response)))

  (:method (response (content null))))
      

(defgeneric http:send-headers (response)
  (:documentation
    "Given a response instance, which has been modified to reflect the intended
    status and headers, emit the response status code, reason phrase, response
    header fields and entity header fields. This should use the response stream
    in its initial :iso-86001 configuration. Once the response head has been
    written, reconfigure the stream to reflect the media type character encoding
    and possible content encoding.")
  (:method :around ((response http:response))
    ;; advance the state - this permits send-headers itself to use the content stream
    ;; without infinite recursion
    (unless (response-state response)
      (setf (response-state response) :headers)
      (multiple-value-prog1 (call-next-method)
        (setf (response-state response) :body)
        ;; configure the response stream. this must be delayed to this point, rather
        ;; than performed as a side-effect of setting the response content type, as
        ;; that would change the encoding for the headers
        (let ((stream (get-response-content-stream response)))
          (setf (http:stream-media-type stream)
                ;; adopt whatever was negotiated - or fall-back if that has not yet happened
                ;; because, eg authentication failed prior to content negotiation
                (or (http:response-media-type response) mime:text/plain))
          (when (http:response-transfer-encoding-header response)
            (setf (chunga:chunked-stream-output-chunking-p stream) t))
          ;; TODO : iff content encoding is specified, wrap the response stream with
          ;; one which pipes through a zip process. need to take a possible ssl 
          ;; wrapper into account.
          )))))


(defgeneric http:response-status-code (response)
  )


(defgeneric (setf http:response-status-code) (code response)
  )

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
     respective serialization entirely and the stream content is correct as-is.
     Always close the stream upon completion."
    ;; nb. for sub-processes, this does not suffice as more needs to be done to dispose of the process
    (unwind-protect (http:copy-stream content-stream (http:response-content-stream response))
      (close content-stream)))

  (:method ((condition http:condition) (response t) (content-type t))
    "Given a condition as the result, just signal it."
    (error condition)))

;;;
;;; generic authentication/authorization functions
;;;
;;; they define the interface signature and must be specialized is used in a
;;; resource function definition

(defgeneric http:authenticate-request-password (resource request)
  (:documentation
    "Attempt to establish the request agent identity based on the request's
    authentication headers, that is based on the user name and password.
    If that succeeds, bind the respective agent instance to the request and
    return true."))


(defgeneric http:authenticate-request-token (resource request)
  (:documentation
    "Attempt to establish the request agent identity
    based on an authentication token. If that succeeds, bind the respective
    agent instance to the request and return true."))


(defgeneric http:authenticate-request-session (resource request)
  (:documentation
    "Attempt to establish a request agent identity
    based on a session identifier. If that succeeds, bind the respective
    agent instance to the request and return true."))

(defgeneric http:authenticate-request-location (resource request)
  (:documentation
    "If it is to be permitted, that a request location is sufficient for authentication
     a method should be defined for (resource request) to create an agent, bind the
     instance to the request and return true."))

(defgeneric http:authenticate-anonymous (resource request)
  (:documentation
    "If it is to be permitted, that a request be anonymous
     a method should be defined for (resource request) to return true.
     The default method returns nil")

  (:method ((resource t) (request t))
    nil))


(defgeneric http:authorize-request (resource request)
  (:documentation
    "Given a resource and a request with an agent bound, determine whether the
    agent is authorized to perform the request's method on the given resource."))





;;; hunchentoot integration
;;;
;;; the standard api offers the levels 
;;; process-connection (acceptor)
;;; -> process-request (request)
;;;    -> handle-request (request)
;;;       -> acceptor-dispatch-request (acceptor request)
;;;          -> .eventual.response.implementation. (request)
;;; the direct implementation would be to specialize acceptor-dispatch-request with the implemention based
;;; on generic functions, but
;;; - this control pattern is perverse on two accounts. first, it dynamicall binds the acceptor in order to reintroduce it as a
;;; specialized argument to acceptor-dispatch-request. second, it does not include the reply object in the
;;; signatures.
;;; - status formatting is static, enumerated code
;;; - process-request implements a baroque mechanism to send headers control start-output
;;;
;;; this implementation replaces these five levels with a three
;;; process-connection (acceptor) : to extract headers, set up streams and establish request and response instances
;;; -> respond-to-request (acceptor request response) : to derive the response function, manage headers, manage errors
;;;    -> .some.dispatch.function. (path request response) : the path interpreation function
;;;       -> .some.resource.function. (resource response function content-type accept-type)
;;;
;;;


;;;
;;; simple validation

(defclass account-resource (http:resource)
  ((account :initarg :account)))
(defclass repository-resource (account-resource)
  ((repository :initarg :repository)))

(unless (and
         (equal (sort (mapcar #'http:resource-pattern-name (add-pattern
                                                            (add-pattern
                                                             (list (make-instance 'http:resource-pattern :name "/:account/protocol" :class t))
                                                             (make-instance 'http:resource-pattern :name "/:account/repositories" :class t))
                                                            (make-instance 'http:resource-pattern :name "/:account/repositories/:repository" :class t)))
                      #'string-lessp)
                '("/:account/protocol" "/:account/repositories"))

         (typep (http:bind-resource (add-pattern
                                     (add-pattern
                                      (list (make-instance 'http:resource-pattern :name "/:account/protocol" :class t))
                                      (make-instance 'http:resource-pattern :name "/:account/repositories" :class 'account-resource))
                                     (make-instance 'http:resource-pattern :name "/:account/repositories/:repository" :class 'repository-resource))
                                    "/asdf/repositories"
                                    :request)
                'account-resource)

         (equal (mapcar #'pattern-wildcard-count
                        (list (make-instance 'http:resource-pattern :name "/:account/repositories" :class t)
                              (make-instance 'http:resource-pattern :name "/:account/repositories/:repository" :class t)))
                '(1 2))

         (let ((pattern (add-pattern
                         (add-pattern
                          (list (make-instance 'http:resource-pattern :name "/:account/protocol" :class t))
                          (make-instance 'http:resource-pattern :name "/:account/repositories" :class 'account-resource))
                         (make-instance 'http:resource-pattern :name "/:account/repositories/:repository" :class 'repository-resource))))
           (and (match-pattern pattern "/an-account/protocol")
                (match-pattern pattern "/an-account/repositories")
                (match-pattern pattern "/an-account/repositories/aa-repository")))
         
         (and (pattern-subsumes-p (make-instance 'http:resource-pattern :name "/:account/repositories" :class t)
                                  (make-instance 'http:resource-pattern :name "/:account/repositories/:repository" :class t))
              (pattern-subsumes-p (make-instance 'http:resource-pattern :name "/:account/repositories" :class t)
                                  (make-instance 'http:resource-pattern :name "/:account/repositories/count" :class t))
              (not (pattern-subsumes-p (make-instance 'http:resource-pattern :name "/:account/repositories/:repository" :class t)
                                       (make-instance 'http:resource-pattern :name "/:account/repositories" :class t)))
              (pattern-subsumes-p (make-instance 'http:resource-pattern :name "/:account/repositories/:repository" :class t)
                                  (make-instance 'http:resource-pattern :name "/:account/repositories/a-repository" :class t))
              (not (pattern-subsumes-p (make-instance 'http:resource-pattern :name "/:account/repositories/a-repository" :class t)
                                       (make-instance 'http:resource-pattern :name "/:account/repositories/:repository" :class t))))
         
         #+(or)
         (and (merge-patterns (make-instance 'http:resource-pattern :name "/:account/repositories" :class t)
                              (make-instance 'http:resource-pattern :name "/:account/repositories/:repository" :class t))
              (merge-patterns (make-instance 'http:resource-pattern :name "/:account/repositories/:repository" :class t)
                              (make-instance 'http:resource-pattern :name "/:account/repositories" :class t))
              (not (merge-patterns (make-instance 'http:resource-pattern :name "/:account/repositories/:repository" :class t)
                                   (make-instance 'http:resource-pattern :name "/:account/protocol" :class t))))
         
         (equal (nth-value 1 (match-pattern (make-instance 'http:resource-pattern :class t :name ":asdf/asdf") '("qwer" "asdf")))
                '(:asdf "qwer")))
  (warn "Some resource pattern validation failed..."))

(setf (find-class 'test-resource) nil)
