## An abstract HTTP implementation:

 In the course of processing an HTTP request, the following sorts of things happen:

 - identify the resource
 - identify the agent
 - evaluate agency respective the resource and request method
 - interpret request metadata (headers and parameters) and content
 - effect the method, which means some operations on, to, or with the resource
 - generate response metadata and content

 This library realizes this protocol in terms of classes, abstract operators,
 condition handlers, and a generic function pattern.
 It mediates between the application, which implements the sorts of things
 above, and a communication library, which provides the communication operations,
 wire-level protocol support, and event mediation.

 - classes: 
   - http:request
   - http:response
   - http:resource
   - http:agent
   - mime:mime-type
 - operators :
   - http:encode-content
   - http:decode-content
 - conditions :
   - errors and exceptions corresponding to http response status codes
 - method-combination:
   - http:http

-------

 The library proposes as the application structure:

 - implement the protocol independent of the concrete application as a method
   combination which constructs the effective response function
   from constituents qualified according to their role in the protocol
   - :auth : authentication and authorization
   - :pre-processing : request content decoding, specialized by media type
   - :get, :put, etc : protocol method
   - :post-processing : response content encoding, specialized by media type
 - riefy all other protocol components
   - resource : combines the literal http request path with slots for extracted
     properties  and with authorization information provided by the application
   - request : represents the request message, in particular the header fields
     and the content stream
   - response : represents the response message, in particular the header
     fields and the content stream
   - agent : encapsulates the agent 
   - mime-type : model the request content type as a single media type.
     model the response type as a type union

 Its design suggests the following principles
 - It should be possible to understand the structure and, better yet, the beahviour
   of an application five years later with just knowledge of the HTTP protocol
   specification and an abstract application declaration.
 - It should be possible to compose the implementation of simultaneously
   active services and modify them dynamically.
 - Base the implementation on a model for the protocol and its porcessing and
   reify all entities in that model.

 A concrete application implements the resource manipulation methods (the :get,
 :put, etc) and either implements ideosyncratic media types or integrates
 standard forms, such as json or xml. The approach accommodates through
 specialization those aspect which vary among aplications and declares their
 interaction by associating each with a static protocol element through the
 method combination. This approach stands in
 in contrast to two of the alternative http protocol implementation frameworks:
 class specialization, and function composition.

 Protocol "class specialization" proposes to tailor the application responses
 by specializing the response class with respect to protocol aspects such as
 authentication, resource-specific dispatching, and content media type. With
 that approach, alternative responses are effected through variations in class
 composition and method delegation. The approach can make it difficult to
 realize an application because all logic must be reduced to a single
 precedence chain. This library suggests the alternative, to realize each 
 static protocol aspects in its own aspect of the response method.

 Protocol "function composition" proposes a "middleware" metaphor for
 ad-hoc function combination as
 the mechanism to compose response functions. With that approach, each protocol
 aspect is implemented in an individual function and the variations in the
 response process are effected through variations in the call graph. For
 example, a trivial application such as

#### express.js [1]

    app.get('/hello.txt', function(req, res){
      var body = 'Hello World';
      res.setHeader('Content-Type', 'text/plain');
      res.setHeader('Content-Length', body.length);
      res.end(body);
    });
    app.listen(3000);

 looks cool, but consider what hapens when one combines authentication, error
 handling, and content negotiation:

    app.use(express.bodyParser());
    app.use(express.methodOverride());
    app.use(app.router);
    app.use(logErrors);
    app.use(clientErrorHandler);
    app.use(errorHandler);


#### ningle [2]

    (defvar *app* (make-instance 'ningle:<app>))

    (setf (ningle:route *app* "/" :method :get)
      "Welcome to ningle!")

    (clack:clackup
      (builder
        <clack-middleware-session>
        *app*))

#### caveman [3]
    (defparameter *web* (make-instance '<app>))

    (defroute ("/" :method :GET) ()
      (with-layout (:title "Welcome to My site")
        (render #P"index.tmpl")))

#### ring [4]
    (defroute about "/about")
    (defroute documents "/docs/:title")

    (compojure/defroutes main-routes
      (compojure/GET about-template request (handlers.root/root-page request)) ;; will use /about as a template
      (compojure/GET documents-template request (handlers.root/documents-page request)) ;; will use /documents as a template
      (route/not-found "Page not found"))

with middleware

    (defn handler [request]
      (prn (get-in request [:body "user"]))
      (response "Uploaded user."))

    (def app
      (wrap-json-body handler {:keywords? true}))

or

    (defn authenticated? [name pass]
      (and (= name "foo")
           (= pass "bar")))

    (def app (-> routes
                 ...
                 (wrap-basic-authentication authenticated?))

#### HTTP Kit [5]
    (:use [compojure.route :only [files not-found]]
          [compojure.handler :only [site]] ; form, query params decode; cookie; session, etc
          [compojure.core :only [defroutes GET POST DELETE ANY context]]
          org.httpkit.server)
    (defn show-landing-page [req] ;; ordinary clojure function, accepts a request map, returns a response map
      ... )
    (defn update-userinfo [req]          ;; ordinary clojure function
      (let [user-id (-> req :params :id)    ; param from uri
            password (-> req :params :password)] ; form param
        ... ))
    (defroutes all-routes
      (GET "/" [] show-landing-page)
      (GET "/ws" [] chat-handler)     ;; websocket
      (GET "/async" [] async-handler) ;; asynchronous(long polling)
      (context "/user/:id" []
               (GET / [] get-user-by-id)
               (POST / [] update-userinfo))
      (route/files "/static/") ;; static file url prefix /static, in `public` folder
      (route/not-found "<p>Page not found.</p>")) ;; all other, return 404

    (run-server (site #'all-routes) {:port 8080})

These all provide the means to freely compose a process to respond to a request,
but the abstract implementation provides incomplete discursive support to ones effort to
comprehend the implementation.
In exchange for the ability to introduce entities which have no immediate counterpart in the protocol -
eg plug-in progress-bars or rate-limits, one must understand the composition rules - for example
`app.use( ... )` and `(:use ... )` and examine the respective component implementations.
conventional operator names can at best hint at the consequences and the effect will be
to conflate resource manipulation and content interpretation and generation with
protocol processing.
the protocol implementation is entrained in the operator definitions and the composition process.

While it may be true, that the definition of any computation devolves to rules to combine
named functions, It would stell be better to separate those two aspects:
implement the operators and then wire them together
by ascribing properties in terms of a model for the protocol.


 This library proposes, that the model for HTTP has two aspects: resources and representation types which
 the implementation maps onto function compositions.
 a web server accepts response specification in the form of resource and representation designators, uses those
 to identify a processes - in the form of a function, activates that function, and depeding on its
 results type, marshals the results as the response. In these terms, the problem is, how to locate functional
 components in a graph of resources and representations.
 This task does not require arbitrary compositions, but rter it suffices to describe the functional components
 in the space of resource and representation specializations and protocol roles.

 Resources exist at locations in an http-iri namespace. these locations are arranged in subsumption relations.
 an elementary form of this is regular expression subsumption. while, in theory, it would be possible to use that
 criteria to derive relations, a simpler path is to declare the relations. which function is applicable depends
 on which are specialized to cover the set of designated resources.

 Representations exist at locations in a media type space and the means to generate a representation for a resource
 are identified according to the media type(s) which they generate. while concrete types are distinct - eg.,
 `application/turtle`, both `application/turtle` and `application/rdf+xml` are rdf document forms and
 the type `application/x-turtle` is the experimental precursor to `application/turtle`, and, as such to be
 processed equivalently, with the exception, that the requested type should be indicated in the response.
 Request types, on the other hand are rarely concrete, but rather speculate as to the types, character sets, and
 encodings supported by a server. Each independent aspects which can affect the response content, must be available
 to designate the specific function(s) to be chosen to respond. With respoect to each aspect, the each media type
 indicates a set of responses and the combined specification is the union of the type, ordered according to
 the range parameter.

 The protocol roles reflect the stages in request processing: authentication, pre-processing, processing, and
 post-processing. according to this approach, a simple application would be describes as

    (defconstant +page-content+ "
    <html>
     <head>
      <title>http test page</title>
      </head>
     <body>
      <h1>test page</h1>
      <p>This is a test for '~a'.</p>
      </body>
     </html>
    ")
    
    (defclass /test/* (http:resource)
      ((target
        :initform nil :initarg :target
        :reader resource-target))
      (:metaclass http:resource-class)
      (:pattern . "/test/([^/]+)")
      (:keywords :target))
    
    (http:def-resource-function respond (resource request response)
      (:get ((resource /test/*) (request t) (response t) (content-type t) (accept-type t))
            (resource-target resource))
      (:post-process ((resource t) (request t) (response http:response) (content-type t) (accept-type mime:text/html))
        (let ((content (call-next-method)))
          (http:send-headers response)
          (http:log :debug *trace-output* +page-content+ content)
          (write-sequence (format nil  +page-content+ content) (http:response-content-stream response)))))
    
    
    (defpackage "127.0.0.1"
      (:use )
      (:import-from :cl-user :respond)
      (:export :respond))

    (defparameter *a*
      (hunchentoot:start (make-instance 'tbnl::tbnl-acceptor :port 8009 :address "127.0.0.1")))


----
[1] : expressjs.com/  
[2] : http://8arrow.org/ningle/  
[3] : https://github.com/fukamachi/caveman/blob/master/README.v2.markdown  
[4] : https://github.com/ring-clojure/ring  
        
[5] : http://http-kit.org/  