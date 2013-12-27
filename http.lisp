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
   http:agent
 three operators
   http:encode-content
   http:decode-content
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
  ((agent
    :initform nil :initarg :agent
    :accessor http:request-agent
    :documentation "Provides for an agent instance to be determined by any
     authorization methods. If no methods are defined, this remains null.")
   (session-cookie-name
    :initform nil :initarg :session-cookie-name
    :accessor http:request-session-cookie-name)
   (media-type
    :accessor request-media-type
    :documentation
    "Binds the reified request content type or NIL if none was specified.
    (See http:request-media-type - note the package)")
   (method
     :initform nil
     :reader get-request-method :writer setf-request-method
     :documentation
     "Binds the effective request method resective over-riding headers.
     (See http:request-method)")
   (accept-type
    :initform nil
    :reader get-request-accept-type :writer setf-request-accept-type
    :documentation "Binds the interned media type which is computed from the
     request accept header and accept-charset header upon first reference.
     This is supplied to the resource function, to be used to discriminate
     response generation methods and to derive the response content type
     from the applicable methods."))
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
  (loop for class-name in (class-direct-superpatterns class)
        for superpattern-class = (find-class class-name nil)
        do (if superpattern-class
             (when (typep superpattern-class 'http:resource-class)
               (pushnew class (class-direct-subpatterns superpattern-class)))
             (warn "superpattern class not found: ~s: ~s" (class-name class) class-name))))

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
  ((identifier
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
     the scope of the generated effective methods."))
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
    :initarg :request
    :reader http:response-request
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
    :accessor http:response-close-stream-p)
   (keep-alive-p
    :initarg :keep-alive-p
    :accessor http:response-keep-alive-p))
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
    (let ((t-class (find-class t)))
      (c2mop:ensure-method function
                           `(lambda (resource request response)
                              (,handler-name resource
                                             request response
                                             (http:request-media-type request)
                                             (http:request-accept-type request)))
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
             (subpattern-p old specializer))
           (new-is-subpattern (old)
             (subpattern-p specializer old)))
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
  specific implementation. Is signals a not-found condition, should no
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
                            (let ((http:*resource* (http:bind-resource (function ,name) resource-path)))
                              (if http:*resource*
                                (,name http:*resource* request response)
                                (http:not-found))))
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
      (http:log :debug *trace-output* "resource-functions: ~a"
                (mapcar #'c2mop:generic-function-name resource-functions))
      (loop for resource-function in resource-functions
            for name = (c2mop:generic-function-name resource-function)
            do (loop for method in (c2mop:generic-function-methods resource-function)
                     for resource-class = (first (c2mop:method-specializers method))
                     when (and (subtypep resource-class root-resource-class)
                               (http-verb-list-p (method-qualifiers method)))
                     do (progn (http:log :debug *trace-output* "adding resource dispatch method: ~a ~a ~s"
                                         name (class-name resource-class) (method-qualifiers method))
                               (http:define-dispatch-method dispatch-function name resource-class))))
      dispatch-function)))


(defgeneric http:bind-resource (specializer path)
  (:documentation
   "Given specializer - a resource-class, and path - an url path string,
   perform a depth-first search through the respective class hierarchy to discover the
   first most specialized class for which the resource pattern matches the path.
   GIven a match, return a new resource instance which binds the register values from the
   regular expression match.")

  (:method ((function http:dispatch-function) (path string))
    (loop for class in (http:function-resource-classes function)
          for resource = (http:bind-resource class path)
          when resource
          return resource))

  (:method ((specializer http:resource-class) (path string))
    (multiple-value-bind (start end starts ends) (cl-ppcre:scan (class-pattern specializer) path)
      (declare (ignore end))
      (when start
        (flet ((search-subpatterns (sub-class) (http:bind-resource sub-class path)))
          (declare (dynamic-extent #'search-subpatterns))
          (or ;; (some #'search-sub-classes (c2mop:class-direct-subclasses specializer))
           (some #'search-subpatterns (class-direct-subpatterns specializer))
              (apply #'make-instance specializer
                     :path path
                     (loop for initarg in (class-keywords specializer)
                           for start across starts
                           for end across ends
                           collect initarg
                           collect (subseq path start end)))))))))


;;;
;;;  request

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


(defgeneric http:request-accept-type (request)
  (:method ((request http:request))
    (or (get-request-accept-type request)
        (let ((accept (remove #\space (http:request-accept-header request))))
          (when accept
            (or (gethash accept (acceptor-header-instances (http:acceptor)))
                (setf (gethash accept (acceptor-header-instances (http:acceptor)))
                      (intern-media-type accept))))))))


(defgeneric http:request-accept-header (request)
  )

(defgeneric http:request-accept-charset (request)
  )

(defgeneric http:request-content-stream (request)
  )

(defgeneric http:request-content-length (request)
  )

(defgeneric http:request-content-type-header (request)
  )

(defgeneric http:request-if-modified-since (request)
  )

(defgeneric http:request-original-method (request)
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

(defgeneric http:request-negotiated-content-encoding (request)
  )

(defgeneric http:request-path (request)
  )

(defgeneric http:request-header (request key)
  )

(defgeneric http:request-post-argument (request key)
  )

(defgeneric http:request-post-arguments (request key)
  )

(defgeneric http:request-query-argument (request key)
  )

(defgeneric http:request-query-arguments (request key)
  )

(defgeneric http:request-remote-ip-address (request)
  (:documentation
    "Given a request, return the remote ip address"))

(defgeneric http:request-session-id (request)
  (:documentation
    "Given a request, return a session id, if present."))


#+(or)                                  ; artifactual
(defmethod intern-media-type (acceptor (request http:request))
    (let ((header (case (http:request-method request)
                    ((:post :put :patch) (or (http:request-content-type-header request) (http:bad-request)))
                    ((:get :head) (or (http:request-accept-header request) "*/*")))))
      (if header
        (intern-media-type acceptor header)
        mime:*/*)))

(defgeneric http:request-unmodified-since (request)
  )

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
                 (http:redirect (redirection)
                                ;; if the redirection is internal invoke it, otherwise resignal it
                                (let ((location (http:condition-location redirection)))
                                  (if (functionp location)
                                    (funcall location)
                                    (error redirection)))))))
        (pprint form)
        form))))

(define-method-combination http:http (&key )
                           ((identification (:auth :identification))
                            (permission (:auth :permission))
                            (around (:around) )
                            (decode (:decode . *)) ;; should be singleton
                            (encode (:encode . *))
                            (get (:get))
                            (put (:put))
                            (head (:head))
                            (patch (:patch))
                            (post (:post))
                            (delete (:delete))
                            (options (:options))
                            (trace (:trace))
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
    (if (and encode (eq (second (method-qualifiers (first encode))) :as))
      `(call-method ,(first encode) ())
      (let* ((form
              `(handler-case
                 ,(compute-effective-resource-function-method function
                                                              identification permission
                                                              around 
                                                              decode
                                                              (qualify-methods)
                                                              encode)
                 (http:redirect (redirection)
                                ;; if the redirection is internal invoke it, otherwise resignal it
                                (let ((location (http:condition-location redirection)))
                                  (if (functionp location)
                                    (funcall location)
                                    (error redirection)))))))
        (pprint form)
        form))))

(defgeneric http:respond-to-option-request (function request response verbs)
  (:documentation "The base method implements the default heade response.
   It collected the verbs from the applicable methods into the 'Allow' header and emits
   the headers.")
  (:method ((function t) (request t) (response http:response) verbs)
    ;; the default method 
    (setf (http:response-content-length response) 0)
    (setf (http:response-allow response) verbs)))


(defun compute-effective-resource-function-method (function identification permission around
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

  (let* ((authentication-clause (if (or identification permission)
                                  ;; require that either the agent is already authenticated - eg from redirection
                                  ;; or that one of the identification methods succeed, and that all of the
                                  ;; permission methods succeed
                                  `(unless (and (or (http:request-agent (http:request))
                                                    ,@(loop for method in identification
                                                            collect `(call-method ,method ())))
                                                ,@(loop for method in permission
                                                        collect `(call-method ,method ())))
                                     (http:unauthorized))))
         ;; the most-specific only
         (decode-methods (or decode-methods '((make-method (http::unsupported-media-type)))))
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
                                      collect `(,key ,(if methods
                                                        `(call-method ,(first methods)
                                                                      ,(append (rest methods)
                                                                               (when (member key '(:patch :post :put))
                                                                                 ;; include decode methods iff the verb supports content
                                                                                 ;; otherwise arrange to return nil
                                                                                 decode-methods
                                                                                 '(make-method nil))))
                                                        '(http:not-implemented))))
                              ;; add an options clause if none is present
                              ,@(unless (getf primary-by-method :options)
                                  `((:options (http:respond-to-option-request ,function (http:request) (http:response)
                                                                              ',(loop for (key nil) on primary-by-method by #'cddr collect key)))))
                              ;; otherwise, it is not implemented
                              (t (http:not-implemented))))
         ;; wrap the decoding an content generation steps with a mechanism to encode the result.
         ;; if the content is null, no output should be generated
         ;; if no method was applicable, generate logic to either derive an alternative concrete media type
         ;; signal a nont-applicable error if that fails.
         (main-clause (if encode-method
                        `(call-method ,encode-method ((make-method ,content-clause)))
                        `(progn
                           (when (or (eq :get (http:request-method http:*request*))
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
                                      (http::not-acceptable)))))
                           ,content-clause))))
    (when mime-type-clause
      (setf main-clause `(progn ,mime-type-clause ,main-clause)))
    (when authentication-clause
      (setf main-clause `(progn ,authentication-clause ,main-clause)))
    (let* ((form (if around
                   `(call-method ,(first around) (,@(rest around) (make-method ,main-clause)))
                   main-clause)))
      form)))



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
                                              (let* ((after-qualifiers (member-if (complement #'keywordp) clause))
                                                     (qualifiers (ldiff clause after-qualifiers)))
                                                (if (consp (first after-qualifiers))
                                                  `(:method ,@clause)
                                                  (ecase (second qualifiers)
                                                    (:as
                                                     `(:method ,@qualifiers ((resource t) (request t) (response t) (content-type t) (accept-type ,(first after-qualifiers)))
                                                        (,name resource request response content-type ,(second after-qualifiers))))
                                                    ((nil)
                                                     `(:method ,@qualifiers ((resource t) (request t) (response t) (content-type t) (accept-type ,(first after-qualifiers)))
                                                        ;; encode as per the derived response content type, which will be an instance of the
                                                        ;; specializer class, but include the character set encoding
                                                        (let ((content (call-next-method))
                                                              (effective-content-type (http:response-media-type response)))
                                                          (format *trace-output* "~%;;; effective-content-type: ~s" effective-content-type)
                                                          (http:encode-response content response effective-content-type))))))))
                                             (:decode
                                              (let* ((after-qualifiers (member-if (complement #'keywordp) clause))
                                                     (qualifiers (ldiff clause after-qualifiers)))
                                                (if (consp (first after-qualifiers))
                                                  `(:method ,@clause)
                                                  `(:method ,@qualifiers ((resource t) (request t) (response t) (content-type ,(first after-qualifiers)) (accept-type t))
                                                     (http:decode-request resource request content-type)))))
                                             (:auth
                                              (if (consp (third clause))
                                                `(:method ,@clause)
                                                `(:method ,@(subseq clause 0 2) ((resource t) (request t) (response t) (content-type t) (accept-type t))
                                                          (,(third clause) resource request))))))))
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
      `(defgeneric ,name ,lambda-list
         (:argument-precedence-order ,(first lambda-list) ,@(subseq lambda-list 3 5) ,@(subseq lambda-list 1 3))
         ,@definition-clauses))))


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


;;;
;;; response

(defgeneric http:response-accept-ranges (ranges response)
  )

(defgeneric (setf http:response-allow) (allow-verbs response)
  (:method ((allow-verbs list) (response http:response))
    (setf (http:response-allow-header response) (format nil "~{~a~^,~}" allow-verbs))))

(defgeneric (setf http:response-allow-header) (allow-verbs response)
  )

(defgeneric (setf http:response-cache-control) (value response)
  )

(defgeneric (setf http:response-content-length) (value response)
  ;; hunchentoot requires an integer argument
  (:method ((value integer) (response http:response)) 
    (setf (http:response-content-length-header response) value)))

(defgeneric (setf http:response-content-length-header) (value response)
  )

(defgeneric http:response-compute-media-type (request response class &key charset)
  (:method ((request t) (response http:response) (type mime:mime-type) &rest args)
    (declare (dynamic-extent args))
    (apply #'mime:mime-type type args)))


(defgeneric http:response-content-stream (response)
  (:documentation "Return the response content stream while ensuring that
   the response head has been sent before the body")
  (:method ((response http:response))
    ;; ensure the headers are sent
    (when (http:response-headers-unsent-p response)
      (http:send-headers response))
    (get-response-content-stream response)))

(defgeneric (setf http:response-content-type-header) (content-type-header response)
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


(defgeneric (setf http:response-content-disposition) (disposition response)
  )

(defgeneric (setf http:response-content-encoding) (content-encoding response)
  )

(defgeneric (setf http:response-character-encoding) (character-encoding response)
  )

(defgeneric (setf http:response-etag) (tag response)
  )

(defgeneric http:response-headers-unsent-p (response)
  (:method ((response http:response))
    (case (response-state response)
      ((nil) t)
      (t nil))))

(defgeneric (setf http:response-last-modified) (timestamp response)
  )

(defgeneric (setf http:response-location-header) (location response)
  )

(defgeneric (setf http:response-www-authenticate-header) (authentication-method response)
  )

(defgeneric (setf http:response-retry-after-header) (time response)
  )

(defgeneric (setf http:response-transfer-encoding-header) (time response)
  )

(defgeneric (setf http:response-vary) (value response)
  )



(defgeneric http:request-etags (request)
  )

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

(defgeneric http:report-condition-headers (condition response)
  (:documentation "Given a condition, assert its side-effects on the request
    state, to be communicated to the client as part of the response.")

  (:method :before ((condition t) response)
    (setf (http:response-content-length response) 0))

  (:method ((condition http:redirect) response)
    (setf (http:response-location-header response) (http:condition-location condition))
    (call-next-method))

  (:method ((condition http:not-modified) response)
    (call-next-method))

  (:method ((condition http:unauthorized) response)
    (setf (http:response-www-authenticate-header response) "Basic")
    (call-next-method))

  (:method ((condition http:internal-error) response)
    (let ((time (http:condition-retry-after condition)))
      (when time
        (setf (http:response-retry-after-header response) time)))
    (call-next-method)))



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
          (setf (http:stream-media-type stream) (http:response-media-type response))
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
;;; they serve as protoypes to document the signature and could be specialized,
;;; but are not required - any function is permitted, so long as it accepts these
;;; arguments and perform the respective task

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



