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
  ((session-cookie-name
    :initform nil :initarg :session-cookie-name
    :accessor http:request-session-cookie-name)
   (content-type
    :accessor request-content-type
    :documentation
    "Binds the reified request content type or NIL if none was specified.
    (See http:request-content-type)")
   (method
     :accessor request-method
     :documentation
     "Binds the effective request method resective over-riding headers.
     (See http:request-method)"))
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
    :reader class-keywords))
  (:documentation
    "The class resource-class is the metaclass for the resource specializations,
    each of which represents an individual http path pattern. It add slots to
    each class to bind the path regular expression pattern and the respective
    keywords to be used to instantiate."))

(eval-when (#+lispworks :compile-toplevel :load-toplevel :execute)
  (defmethod c2mop:validate-superclass ((subclass http:resource-class)
					(superclass standard-class))
    t))

(defmethod shared-initialize ((instance http:resource-class) (slots t) &rest initargs
                              &key pattern)
  (when pattern
    (setf initargs (copy-list initargs))
    (setf (getf initargs :pattern)
          (cl-ppcre:create-scanner pattern :case-insensitive-mode nil)))
  (apply #'call-next-method instance slots
         initargs))


(defclass http:resource ()
  ((path
    :initarg :path
    :reader http:resource-path)
   (authorization-list
    :initarg :authorization-list
    :accessor resource-authorization-list))
  (:metaclass http:resource-class)
  (:pattern )
  (:keywords ))


(defclass http:resource-function (standard-generic-function)
  ((method-keys
    :initform '(:get :put :head :patch :post :delete :options :trace :connect) :initarg :method-keys
    :accessor http:function-method-keys))
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
   (content-stream
    :initarg :content-stream
    :reader get-response-content-stream :writer (setf http:response-content-stream))
   (server-protocol
    :initarg :server-protocol
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
                                             (http:request-content-type request) (http:request-accept-type request)))
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
             response)))


;;;
;;; dispatch function

(defmethod add-method :after ((function http:dispatch-function) method)
  "As a side effect of adding a method, integrate its path specializer into
 the generic function's interning discrimination network."
  (let ((class (first (c2mop:method-specializers method))))
    (when (typep class 'http:resource-class)
      (add-resource-class function class))))

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
                            (let ((resource (http:bind-resource (function ,name) resource-path)))
                              (if resource
                                (,name resource request response)
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
        (flet ((search-sub-classes (sub-class) (http:bind-resource sub-class path)))
          (declare (dynamic-extent #'search-sub-classes))
          (or (some #'search-sub-classes (c2mop:class-direct-subclasses specializer))
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

(defgeneric http:request-session-id (request)
  (:documentation
    "Given a request, return a session id, if present.")
  )

(defgeneric http:authenticate-request-password (resource request)
  (:documentation
    "Attempt to establish the request agent identity based on the request's
    authentication headers, that is based on the user name and password.
    If that succeeds, bind the respective agent instance to the request."))


(defgeneric http:authenticate-request-token (resource request)
  (:documentation
    "Attempt to establish the request agent identity
    based on an authentication token. If that succeeds, bind the respective
    agent instance to the request."))


(defgeneric http:authenticate-request-session (resource request)
  (:documentation
    "Attempt to establish a requet agent identity
    based on a session identifier. If that succeeds, bind the respective
    agent instance to the request."))


(defgeneric http:authorize-request (resource request)
  (:documentation
    "Given a resource and a request with an agent bound, determine whether the
    agent is authorized to perform the request's method on the given resource."))


(defgeneric http:request-accept-type (request)
  (:method ((request http:request))
    (let ((header (http:request-accept-header request)))
      (when header (intern-media-type (http:request-acceptor request) request)))))

(defgeneric http:request-accept-header (request)
  )

(defgeneric http:request-content-stream (request)
  )

(defgeneric http:request-content-length (request)
  )

(defgeneric http:request-content-type-header (request)
  )

(defgeneric http:request-content-type (request)
  (:method ((request http:request))
    (if (slot-boundp request 'content-type)
      (request-content-type request)
      (setf (request-content-type request)
            (let ((header (http:request-content-type-header request)))
              (when header
                (mime:mime-type header)))))))

(defgeneric http:request-line-method (request)
  )

(defgeneric http:request-method (request)
  (:method ((request http:request))
    (if (slot-boundp request 'content-type)
      (request-method request)
      (setf (request-method request)
            (flet ((as-method-key (string)
                     (or (find-symbol (string-upcase string) *http-method-package*)
                         (http:not-implemented :method string))))
              (let ((header-method (http:request-header request :x-http-method-override)))
                (if header-method
                  (as-method-key header-method)
                  (if (and (eq (http:request-method request) :post)
                           (eq (http:request-content-type request) mime:application/x-www-form-urlencoded))
                    (let ((post-method (http:request-post-argument request :_method)))
                      (if post-method
                        (as-method-key post-method)
                        (http:request-line-method request)))))))))))

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

(defgeneric http:query-field-value (request key)
  )

(defmethod intern-media-type (acceptor (request http:request))
    (let ((header (case (http:request-method request)
                    ((:post :put :patch) (or (http:request-content-type-header request) (http:bad-request)))
                    ((:get :head) (or (http:request-accept-header request) "*/*")))))
      (if header
        (intern-media-type acceptor header)
        mime:*/*)))


;;;
;;; def-resource

(defmacro http:def-resource ((name pattern &rest keywords) parent-classes slots &rest options)
  (unless (assoc :metaclass options)
    (push '(:metaclass http:resource-class) options))
  `(defclass ,name ,(or parent-classes '(http:resource))
     ,slots
     (:pattern . ,pattern)
     (:keywords ,@keywords)
     ,@options))

#+digitool
(setf (ccl:assq 'http:def-resource ccl:*fred-special-indent-alist*) 2)

;;;
;;; resource-function method combination

(define-method-combination http:http (&key )
                           ((identification (:auth :identification))
                            (permission (:auth :permission))
                            (around (:around) )
                            (pre (:pre-process . *))
                            (post (:post-process . *))
                            (primary http-verb-list-p))
  (:arguments resource request-arg response content-type)
  (:generic-function function)
  (flet ((qualify-methods (methods)
           (let ((categorized ())
                 (method-keys (http:function-method-keys function)))
             (loop for method in methods
                   do (loop with qualifiers = (method-qualifiers method)
                            for method-key in (if (null (set-difference qualifiers '(:pre-process :post-process)))
                                                method-keys
                                                qualifiers)
                            do (case method-key
                                 ((:pre-process :post-process) )
                                 (* (loop for method-key in method-keys
                                          do (push method (getf categorized method-key))))
                                 (t (push method (getf categorized method-key))))))
             (loop for (key methods) on categorized by #'cddr
                   collect key collect (reverse methods)))))
    `(handler-case (progn (http:log :debug *trace-output* "~a" (list ,resource ,request-arg ,response ,content-type))
                          ,(compute-effective-resource-function-method function request-arg
                                                                       identification permission
                                                                       around 
                                                                       (qualify-methods pre)
                                                                       (qualify-methods primary)
                                                                       (qualify-methods post)))
       (http:redirect (redirection)
         ;; if the redirection is internal invoke it, otherwise resignal it
         (let ((location (http:condition-location redirection)))
           (if (functionp location)
             (funcall location)
             (error redirection)))))))


(defun compute-effective-resource-function-method (function request identification permission around
                                                           pre-by-method primary-by-method post-by-method)
  ;; arrange the methods to effect authentication, generate content, and encode it.
  ;; in addition wrap/pre/post process with before/after/round methods
  ;;
  ;; 1. around wraps everything
  ;; 2. execute authentication methods (unordered) as an (or ...) and require a true result.
  ;;    if not, signal not-authorized
  ;; 3. compute the accept union for the media type, character encoding and content encoding
  ;; 4. compute the effective before/after with the method case
  ;; 5. apply the encoding auxiliary to the result of the method-case/before/after and the accepted media type
  (let* ((authentication-clause (if (or identification permission)
                                  `(unless (and (or ,@(loop for method in identification
                                                            collect `(call-method ,method ())))
                                                ,@(loop for method in permission
                                                        collect `(call-method ,method ())))
                                     (http:unauthorized))))
         (content-method-clauses (loop for key in (http:function-method-keys function)
                                       for pre-methods = (or (getf pre-by-method key)
                                                             (when (member key '(:post :put :patch)) '((make-method (http::unsupported-media-type)))))
                                       ;; for each given verb, require a primary method, should the verb appear
                                       for primary-methods = (or (getf primary-by-method key)
                                                                 '((make-method (http:not-implemented))))
                                       for post-methods = (or (getf post-by-method key)
                                                              (when (member key '(:post :put :patch :get :head))
                                                                `((make-method (when (http:request-header :accept ,request) (http::not-acceptable))))))
                                       for method-list = (append post-methods primary-methods pre-methods)
                                       collect `(,key ,@method-list)))
         (main-clause `(case (http:request-method ,request)
                         ,@(loop for (method-key . method-list) in content-method-clauses
                                 collect `(,method-key (call-method ,(first method-list) ,(rest method-list))))
                         (t (http:not-implemented)))))
    (when authentication-clause
      (setf main-clause `(progn ,@(when authentication-clause (list authentication-clause)) ,main-clause)))
    (let* ((form (if around
                   `(call-method ,(first around) (,@(rest around) (make-method ,main-clause)))
                   main-clause)))
      form)))

;;; (compute-effective-resource-function-method 'request '(basic session) '(around) '(:get (get1 get2) :put (put1) :post (post1)) '(:get (test/html text/plain) :put (put-form) :post (post-form)))


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
                                             ((:post-process :post-processing)
                                              (let* ((after-qualifiers (member-if (complement #'keywordp) clause))
                                                     (qualifiers (cons :post-process (rest (ldiff clause after-qualifiers)))))
                                                (if (consp (first after-qualifiers))
                                                  `(:method ,@clause)
                                                  `(:method ,@qualifiers ((resource t) (request t) (response t) (content-type ,(first after-qualifiers)) (accept-type t))
                                                     (http:encode-response (call-next-method) response content-type)))))
                                             ((:pre-process :pre-processing)
                                              (let* ((after-qualifiers (member-if (complement #'keywordp) clause))
                                                     (qualifiers (cons :pre-process (rest (ldiff clause after-qualifiers)))))
                                                (if (consp (first after-qualifiers))
                                                  `(:method ,@clause)
                                                  `(:method ,@qualifiers ((resource t) (request t) (response t) (content-type t) (accept-type ,(first after-qualifiers)))
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


;;;
;;; response

(defgeneric http:response-accept-ranges (ranges response)
  )

(defgeneric http:response-cache-control (value response)
  )

(defgeneric http:response-content-stream (response)
  (:method ((response http:response))
    (cond ((get-response-content-stream response))
          (t
           ;; if it was not configured, then send the headers and try again
           (http:send-headers response)
           (get-response-content-stream response)))))

(defsetf http:response-media-type-header (response) (content-type character-encoding)
  `(values (setf (http:response-content-type ,response) ,content-type)
           (setf (http:response-character-encoding ,response) ,character-encoding)))

(defgeneric (setf http:response-content-type) (content-type response)
  )

(defgeneric (setf http:response-content-encoding) (content-encoding response)
  )

(defgeneric (setf http:response-character-encoding) (character-encoding response)
  )

(defgeneric (setf http:response-etag) (tag response)
  )

(defgeneric (setf http:response-last-modified) (timestamp response)
  )

(defgeneric (setf http:response-location-header) (location response)
  )

(defgeneric (setf http:response-www-authenticate-header) (authentication-method response)
  )

(defgeneric (setf http:response-retry-after-header) (time response)
  )

(defgeneric http:response-vary (value response)
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

  (:method ((condition http:redirect) response)
    (setf (http:response-location-header response) (http:condition-location condition))
    (call-next-method))

  (:method ((condition http:not-modified) response)
    (setf (http:response-media-type-header response) (values mime:text/plain "UTF-8"))
    (call-next-method))

  (:method ((condition http:unauthorized) response)
    (setf (http:response-www-authenticate-header response) "Basic")
    (call-next-method))

  (:method ((condition http:internal-error) response)
    (let ((time (http:condition-retry-after condition)))
      (when time
        (setf (http:response-retry-after-header response) time)))
    (call-next-method)))






(defgeneric http:response-headers-sent-p (response)
  (:method ((response http:response))
    (slot-boundp response 'content-stream)))

(defgeneric http:send-headers (response)
  (:documentation
    "Given a response instance, which has been modified to reflect the intended
    status and headers, create a character stream and emit the response status
    code, reason phrase, response header fields and entity header fields through
    it. As per the response content type and length, save that character stream
    or the original binary stream, possibly augmented with a chunking wrapper
    as the response content stream."))

(defgeneric http:send-entity-body (response body)
  (:documentation
    "Send a monolithic response body.")

  (:method :before ((response http:response) (body t))
    (unless (http:response-headers-sent-p response)
      (http:send-headers response)))

  (:method (response (content null))))


(defgeneric http:response-status-code (response)
  )


(defgeneric (setf http:response-status-code) (code response)
  )


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



