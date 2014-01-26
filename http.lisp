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
    :initform (error "dispatch-function is required.") :initarg :dispatch-function
    :reader http:acceptor-dispatch-function
    :documentation
    "An acceptor invokes this function for each request path. It interns the
    path and dispatches to the combination of path, request and response
    instances to the concrete implementation. Where no value is provided, the
    acceptor defines a generic operator with dispatch methods for each
    implementation method found.")
   (header-instances
    :initform (make-hash-table :test 'equalp)
    :reader acceptor-header-instances))
  (:default-initargs
    :dispatch-function-class 'http:dispatch-function)
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
  ((resource-classes
    :initform nil :initarg :resource-classes
    :accessor http:function-resource-classes
    :documentation
    "Binds a list of the top bound resource classes as the discrimination
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


(defclass http:resource-class (standard-class)
  ((pattern
    :initform (error "pattern is required.") :initarg :pattern
    :reader class-pattern)
   (keywords
    :initform (error "keywords is required") :initarg :keywords
    :reader class-keywords)
   (direct-superpatterns
    :initform nil :initarg :direct-superpatterns
    :reader class-direct-superpatterns
    :documentation "A list of the resource classes for which this one is to be
     tested against the request resource path for a more specific match. Likely
     just a single element, but multiple is possible.
     The value is specified as an additional class initarg in the definition
     form.")
   (direct-subpatterns
    :initform nil
    :accessor class-direct-subpatterns
    :documentation "Collects the inverse relation to the declared
     class-direct-superpatterns. Used to contruct the matching graph for
     resource paths."))
  (:documentation
    "The class resource-class is the metaclass for the resource specializations,
    each of which represents an individual http path pattern. It add slots to
    each class to bind the path regular expression pattern and the respective
    keywords to be used to instantiate."))

(eval-when (#+lispworks :compile-toplevel :load-toplevel :execute)
  (defmethod c2mop:validate-superclass ((subclass http:resource-class)
					(superclass standard-class))
    t)
  (defmethod c2mop:validate-superclass ((superclass standard-class)
					(subclass http:resource-class))
    t))

(defmethod shared-initialize ((instance http:resource-class) (slots t) &rest initargs
                              &key pattern)
  (when pattern
    (setf initargs (copy-list initargs))
    (setf (getf initargs :pattern)
          (cl-ppcre:create-scanner pattern :case-insensitive-mode nil)))
  (apply #'call-next-method instance slots
         initargs))

(defmethod c2mop:finalize-inheritance :after ((class http:resource-class))
  (finalize-pattern-subsumption class))

(defgeneric finalize-pattern-subsumption (class)
  (:documentation "Ensure that this class and all of its sub-classes are included among
    patterns tested by the respective super-pattern class. The mop definition for
    add-direct-subclass specifices that the direct relation should exists for all
    _initialized_ classes, even prior to finalization.")
  (:method ((class http:resource-class))
    ;; use the class general method to involve sub-classes
    (call-next-method)
    ;; link this class' pattern with the more general
    (loop for class-name in (class-direct-superpatterns class)
          for superpattern-class = (find-class class-name nil)
          do (if superpattern-class
               (when (typep superpattern-class 'http:resource-class)
                 (pushnew class (class-direct-subpatterns superpattern-class)))
               (warn "superpattern class not found: ~s: ~s" (class-name class) class-name))))
  (:method ((class standard-class))
    ;; ensure pattern registration for sub-classes to permit them to render a method
    ;; specialized for this class applicable
    (loop for sub-class in (c2mop:class-direct-subclasses class)
          do (finalize-pattern-subsumption sub-class))))

(defgeneric subpattern-p (class1 class2)
  (:method ((class1 http:resource-class) (class2 http:resource-class))
    (or (find class1 (class-direct-superpatterns class2))
        (loop for class in (class-direct-superpatterns class2)
              when (subpattern-p class1 class)
              return t)))
  (:method ((class1 symbol) (class2 t))
    (and class1 (subpattern-p (find-class class1) class2)))
  (:method ((class1 t) (class2 symbol))
    (and class2 (subpattern-p class1 (find-class class2))))
  (:method ((class1 t) (class2 t))
    nil))
  

(defclass http:resource ()
  ((request
    :initarg :request
    :accessor http:resource-request
    :documentation "caches the respective request for access to request parameters")
   (identifier
    :initarg :identifier
    :reader http:resource-identifier
    :documentation "the absolute iri which designates the resource")
   (path
    :initarg :path
    :reader http:resource-path
    :documentation "the path components of the resource identifer which
     contribute to its classification.")
   (authorization-list
    :initarg :authorization-list
    :accessor resource-authorization-list))
  (:metaclass http:resource-class)
  (:pattern )
  (:keywords ))


(defclass http:resource-function (standard-generic-function)
  ((method-keys
    :initform '() :initarg :method-keys
    :accessor http:function-method-keys
    :documentation "Collects the http method keys present in the function's methods. This limits
     the scope of the generated effective methods.")
   (default-accept-header
     :initform "*/*" :allocation :class
     :type string
     :reader http:function-default-accept-header
     :documentation "The accept header value to be used when a request includes no accept header.
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
    :initarg :content-stream :initform (error "content-stream is required.")
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


(defmethod initialize-instance ((instance http:acceptor) &rest initargs
                                &key
                                host
                                (address host)
                                (package (when address (find-package address)))
                                (dispatch-function nil)
                                (dispatch-function-class nil)
                                (resource-function-class nil))
  (declare (dynamic-extent initargs))
  (apply #'call-next-method instance
           :dispatch-function (or dispatch-function
                                  (let* ((name (intern address package))
                                         (function-args `(,@(when dispatch-function-class
                                                              `(:generic-function-class ,dispatch-function-class))
                                                          ,@(when resource-function-class
                                                              `(:resource-function-class ,resource-function-class))
                                                          ,@(when package 
                                                              `(:package ,package)))))
                                    (apply #'ensure-dispatch-function name
                                           function-args)))
           initargs))


;;;
;;; operators


;;;
;;; acceptor

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
    "Generate a dispatch method for the respoective resource class which will invoke the function which includes
     the method for that class. Indirect through funcall-resource-function, which derives the composite accept
     type from the accept header"
    (let ((t-class (find-class t)))
      (c2mop:ensure-method function
                           `(lambda (resource request response)
                              (funcall-resource-function ,handler-name resource
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
  "As a side effect of adding a method, integrate its path specializer into
 the generic function's interning discrimination network."
  (let ((class (first (c2mop:method-specializers method))))
    (when (typep class 'http:resource-class)
      (add-resource-class function class))))

#+(or) ;; type based
(defgeneric add-resource-class (function specializer)
  (:method ((function http:dispatch-function) (specializer http:resource-class))
    (flet ((old-is-subtype (old)
             (subtypep old specializer))
           (new-is-subtype (old)
             (subtypep specializer old)))
      (declare (dynamic-extent #'old-is-subtype #'new-is-subtype))
      (unless (some #'new-is-subtype (http:function-resource-classes function))
        (setf (http:function-resource-classes function)
              (cons specializer (remove-if #'old-is-subtype (http:function-resource-classes function))))))))

(defgeneric add-resource-class (function specializer)
  (:method ((function http:dispatch-function) (specializer http:resource-class))
    (flet ((old-is-subpattern (old)
             (or (eq old specializer)
                 (subpattern-p old specializer)))
           (new-is-subpattern (old)
             (or (eq specializer old)
                 (subpattern-p specializer old))))
      (declare (dynamic-extent #'old-is-subpattern #'new-is-subpattern))
      (unless (some #'new-is-subpattern (http:function-resource-classes function))
        (setf (http:function-resource-classes function)
              (cons specializer (remove-if #'old-is-subpattern (http:function-resource-classes function))))))))


(defgeneric update-resource-classes (function)
  (:method ((function http:dispatch-function))
    (setf (http:function-resource-classes function) nil)
    (loop for method in (c2mop:generic-function-methods function)
          for specializer = (first (c2mop:method-specializers method))
          when (typep specializer 'http:resource-class)
          do (add-resource-class function specializer))))

(defun ensure-dispatch-function (name &key package
                                      resource-function-class
                                      (generic-function-class 'http:dispatch-function))
  "Define a generic function to use as a server's dispatch mediator for
  request resources. The initial operator includes a method which accepts path
  strings, interns them as resources, and recurses to invoke the resource
  specific implementation. It signals a not-found condition, should no
  dispatch method match the path."

  (let* ((function (ensure-generic-function name
                                            :lambda-list '(resource request response)
                                            :documentation (format nil "the request dispatch function for package '~a'" package)
                                            :generic-function-class generic-function-class))
         (t-class (find-class 't)))
    (setf (http:function-resource-classes function) nil)
    (when resource-function-class
      (setf (http:function-resource-function-class function) resource-function-class))
    (when package
      (setf (http:function-package function) package))
    ;; add a method for strings which interns them as resources or, failing that signals 
    ;; a not-found condition
    (c2mop:ensure-method function
                         `(lambda (resource-path request response)
                            (let ((http:*resource* (http:bind-resource (function ,name) resource-path request)))
                              (cond (http:*resource*
                                     (,name http:*resource* request response))
                                    (t
                                     (http:not-found "No resource defiend for path: ~s." resource-path)))))
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
    In addition, introduce a method to accept and intern the resource path and,
    should that dail because the path did not match the resource class for any
    defined method, to signal a not-found exception.

    In order to construct the dispatch logic, add one method for each path class
    to the dispatch function, and integrate its resource specializer into a
    discrimination net, of which each node of the hierarchy binds a regular
    expression which determines membership of a given resource identifier.")

  (:method ((acceptor http:acceptor))
    (compute-dispatch-methods (http:acceptor-dispatch-function acceptor)))

  (:method ((dispatch-function http:dispatch-function))
    (let* ((resource-functions (loop for symbol being each external-symbol in (http:function-package dispatch-function)
                                     for function = (when (fboundp symbol) (symbol-function symbol))
                                     when (http:resource-function-p function)
                                     collect function))
           (root-resource-class (http:function-resource-class dispatch-function)))
      (http:log-debug *trace-output* "resource-functions: ~a"
                      (mapcar #'c2mop:generic-function-name resource-functions))
      (loop for resource-function in resource-functions
            for name = (c2mop:generic-function-name resource-function)
            do (loop for method in (c2mop:generic-function-methods resource-function)
                     for resource-class = (first (c2mop:method-specializers method))
                     when (and (subtypep resource-class root-resource-class)
                               (http-verb-list-p (method-qualifiers method)))
                     do (progn (http:log-debug *trace-output* "adding resource dispatch method: ~a ~a ~s"
                                               name (class-name resource-class) (method-qualifiers method))
                               (http:define-dispatch-method dispatch-function name resource-class))))
      dispatch-function)))


(defgeneric http:bind-resource (specializer path request)
  (:documentation
   "Given specializer - a resource-class, and path - an url path string,
   perform a depth-first search through the respective class hierarchy to discover the
   first most specialized class for which the resource pattern matches the path.
   GIven a match, return a new resource instance which binds the register values from the
   regular expression match.")

  (:method ((function http:dispatch-function) (path string) request)
    (loop for class in (http:function-resource-classes function)
          for resource = (http:bind-resource class path request)
          when resource
          return resource))

  (:method ((specializer http:resource-class) (path string) request)
    (multiple-value-bind (start end starts ends) (cl-ppcre:scan (class-pattern specializer) path)
      (declare (ignore end))
      (when start
        (flet ((search-subpatterns (sub-class) (http:bind-resource sub-class path request)))
          (declare (dynamic-extent #'search-subpatterns))
          (or ;; (some #'search-sub-classes (c2mop:class-direct-subclasses specializer))
           (some #'search-subpatterns (class-direct-subpatterns specializer))
              (apply #'make-instance specializer
                     :path path
                     :request request
                     (loop for initarg in (class-keywords specializer)
                           for start across starts
                           for end across ends
                           collect initarg
                           collect (subseq path start end)))))))))


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
    (and (find etag (http:request-etags request) :test #'equalp)
         (loop for request-time in (http:request-if-modified-since request)
               unless (<= time request-time)
               return nil)
         (loop for request-time in (http:request-unmodified-since request)
               when (<= time request-time)
               return nil)
         t)))

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

(defgeneric http:request-host (request)
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
                (mime:mime-type header)))))))

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

;;;
;;; def-resource

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

(define-method-combination http:http (&key )
                           ((authenticate-password (:auth http:authenticate-request-password))
                            (authenticate-token (:auth http:authenticate-request-token))
                            (authenticate-session (:auth http:authenticate-request-session))
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
                            (trace (:trace))
                            (connect (:connect))
                            (multiple http-verb-list-p))
  (:arguments resource request response content-type accept-type)
  (:generic-function function)
  ;; shut the compiler up
  (declare (ignorable resource request response content-type accept-type))
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
                                                                               (append authenticate-password authenticate-token authenticate-session)
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
      `(progn ,@(when (log-level-qualifies? :trace)
                  `((log-http-function ',(c2mop:generic-function-name function)
                                       ,resource ,request ,response ,content-type ,accept-type
                                       ',(append authenticate-password authenticate-token authenticate-session
                                                 authorize-request
                                                 auth-around
                                                 around 
                                                 decode
                                                 (reduce #'append (remove-if #'keywordp verb-methods))
                                                 encode))))
              ,form))))

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
  
  1. around wraps everything
  2. execute authentication methods (unordered) as an (or ...) and require a true result.
     if not, signal not-authorized
  3. compute the response media type from the most specific encoding method specializer
     and the requested character encoding and cache it in the response stream
  ;; 4--7 cannot happen here as, eg when the encoding method is to emit a string, it should set
  ;; the content length header, which must be emitted and would suppress chunking
  ;; 4. if a content encoding is required, interpose a zip stage (nyi)
  ;; 5. execute the combined content and decoding methods
  ;; 6. configure the response stream
  ;; 7. ensure that the headers are emitted
  5. apply the encoding auxiliary to the result content methods"

  (let* ((authentication-clause (if (or authentication authorization)
                                  ;; require that either the agent is already authenticated - eg from redirection
                                  ;; or that one of the identification methods succeed, and that all of the
                                  ;; permission methods succeed
                                  `(unless (and (or (http:request-agent (http:request))
                                                    ,@(loop for method in authentication
                                                            collect `(call-method ,method ()))
                                                    (http:anonymous-resource-p (http:resource)))
                                                ,@(loop for method in authorization
                                                        collect `(call-method ,method ())))
                                     (http:unauthorized))))
         ;; the most-specific only
         (encode-method (first encode-methods))
         ;; add a clause to cache the concrete media type according to the most
         ;; specific class specializer applicable to the request's accept type
         ;; carrying over the character encoding from the request
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
                                                   '(http:not-implemented))))
                              ;; add an options clause if none is present
                              ,@(unless (getf primary-by-method :options)
                                  `((:options (http:respond-to-option-request ,function (http:request) (http:response)
                                                                              '(:options ,@(loop for (key nil) on primary-by-method by #'cddr collect key))))))
                              ;; otherwise, it is not implemented
                              (t (http:not-implemented))))
         ;; wrap the decoding an content generation steps with a mechanism to encode the result.
         ;; if the content is null, no output should be generated
         ;; if no method was applicable, generate logic to either derive an alternative concrete media type
         ;; signal a nont-applicable error if that fails.
         (main-clause (if encode-method
                        `(call-method ,encode-method ((make-method ,content-clause)))
                        content-clause)))
    (when mime-type-clause
      (setf main-clause `(progn ,mime-type-clause ,main-clause)))
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
    (let* ((form (if around
                   `(call-method ,(first around) (,@(rest around) (make-method ,main-clause)))
                   main-clause)))
      ;; ensure the headers are sent
      (setf form `(progn
                    ,form
                    (http:send-headers (http:response))))
      )))

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
         (definition-clauses (loop for clause in clauses
                                   for key = (first clause)
                                   collect (ecase key
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
                                              (if (consp (second clause))
                                                ;; literal definition
                                                `(:method ,@clause)
                                                (let ((qualifiers (remove-if-not #'keywordp clause))
                                                      (media-types (remove-if #'keywordp clause)))
                                                  (ecase (second qualifiers)
                                                    (:as
                                                     `(:method ,@qualifiers ((resource t) (request t) (response t) (content-type t) (accept-type ,(first media-types)))
                                                        (,name resource request response content-type ,(second media-types))))
                                                    ((nil)
                                                     `(:method ,@qualifiers ((resource t) (request t) (response t) (content-type t) (accept-type ,(first media-types)))
                                                        ;; encode as per the derived response content type, which will be an instance of the
                                                        ;; specializer class, but include the character set encoding
                                                        (let ((content (call-next-method))
                                                              (effective-content-type (http:response-media-type response)))
                                                          ;; (format *trace-output* "~%;;; effective-content: ~s" content)
                                                          ;; (format *trace-output* "~%;;; effective-content-type: ~s" effective-content-type)
                                                          (http:encode-response content response effective-content-type))))))))
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
      `(defparameter ,name
         (defgeneric ,name ,lambda-list
           (:argument-precedence-order ,(first lambda-list) ,@(subseq lambda-list 3 5) ,@(subseq lambda-list 1 3))
           ,@definition-clauses)))))

(defgeneric funcall-resource-function (function resource request response content-type accept-type)
  (:method ((function symbol) (resource t) (request t) (response t) (content-type t) (accept-header t))
    (funcall-resource-function (cond ((boundp function) (symbol-value function))
                                     ((fboundp function) (symbol-function function))
                                     (t (error "undefined resource-function: ~s." function)))
                               resource request response content-type accept-header))
  (:method ((function generic-function) (resource t) (request http:request) (response t) (content-type t) (accept-header t))
    "call the function with its computed acceptable response content type"
    (let ((media-type (resource-function-acceptable-media-type function accept-header)))
      (setf (http:request-accept-type request) media-type)
      (funcall function resource request response content-type media-type))))


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

(defgeneric resource-function-acceptable-media-types (function accept-types)
  (:documentation "Return the ordered sequence of those of the functions encoding media type specializers
   which are subtypes of the given list of accept types. The order is respective the accept types and any
   duplicates are removed from the end.")
  (:method ((function http:resource-function) (accept-types list))
    (remove-duplicates (loop with defined-types = (resource-function-media-types function)
                             for accept-type in accept-types
                             append (loop for defined-type in defined-types
                                          when (or (eq defined-type accept-type)
                                                   (subtypep defined-type accept-type))
                                          collect defined-type))
                       :from-end t)))                


(defgeneric resource-function-acceptable-media-type (function candidate-types)
  (:documentation "Combine the media types from the request accept header with those defined for the function
    to derive a composite type compatible with the function's methods. If none are compatible, yield
    a literal combination, which will then result in a not-acceptable error.")
  
  (:method ((function http:resource-function) (accept-media-type mime:mime-type))
    "Given a mime type instance, return it."
    accept-media-type)
  
  (:method ((function http:resource-function) (accept-specification null))
    "Absent an exxept header, use the functin's default."
    (resource-function-acceptable-media-type function (http:function-default-accept-header function)))

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
    (assert (every #'symbolp accept-specification) ()
            "Invalid accept specification: ~s." accept-specification)
    (let* ((acceptable-type-list (or (resource-function-acceptable-media-types function accept-specification)
                                     ;; if none match, fall-back to that provided, which should yield a not-acceptable error
                                     accept-specification))
           (class-name (intern (format nil "~{~a~^,~}" acceptable-type-list) :mime))
           (media-type-class (or (find-class class-name nil)
                                 (c2mop:ensure-class class-name :direct-superclasses acceptable-type-list))))
      (make-instance media-type-class))))

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
;;; response

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

(defgeneric http:response-compute-media-type (request response class &key charset)
  (:method ((request t) (response http:response) (type mime:mime-type) &rest args)
    (declare (dynamic-extent args))
    (apply #'mime:mime-type type args)))

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
    (null (http:stream-header-stream (http:response-content-stream response)))))

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
          (format nil "~a~@[; charset=~a~]" (type-of media-type) (mime:mime-type-charset media-type)))
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
    (setf (http:response-content-length response) 0)
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
    (format (http:response-content-stream response) "~%~a~%" condition)))


(defgeneric http:reset-headers (response)
  (:documentation "Reset any buffered headers which are pending output. This will
    occur when an error occures during response processing, yet the headers have not
    yet been written.")
  (:method ((response http:response))
    (let ((header-stream (http:response-content-stream response)))
      (if header-stream
        (http:stream-reset-header-stream header-stream)
        (http:log *lisp-errors-log-level* http:*acceptor*
                  "Attempt to reset sent headers")))))

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
      

(defgeneric http:send-entity-body (response body)
  (:documentation
    "Send a monolithic response body.")

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


(defgeneric http:response-status-code (response)
  )


(defgeneric (setf http:response-status-code) (code response)
  )


;;;
;;; example authentication/authorization functions
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
    "Attempt to establish a requet agent identity
    based on a session identifier. If that succeeds, bind the respective
    agent instance to the request and return true."))


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
;;;       -> .sone.resource.function. (resource response function content-type accept-type)
;;;
;;;



