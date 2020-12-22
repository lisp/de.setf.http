;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: HUNCHENTOOT; -*-
;;; Copyright (c) 2004-2010, Dr. Edmund Weitz.  All rights reserved.
;;; Copyright 2013 [james anderson](mailto:james.anderson@setf.de) All Rights Reserved

(in-package :hunchentoot)

;; facilitate specialization
(de.setf.utility:modpackage :hunchentoot
  (:export tbnl-request
           tbnl-response))

;;; hunchentoot adaptations and extensions to integrate generic request management

(defgeneric taskmaster-report-tasks (taskmaster)
  (:method ((taskmaster taskmaster))
    nil))

(defclass tbnl-request (http:request request)
  ())

(defclass tbnl-response (http:response reply)
  ())

(defclass tbnl-acceptor (http:acceptor acceptor)
  ((reply-class :initarg :response-class))
  (:default-initargs
    :request-class 'tbnl-request
    :response-class 'tbnl-response)
  (:documentation "mix the http:acceptor into the base acceptor class in order to get
   - dispatch function computation when the acceptor is instantiated
   "))

;;; acceptor

(defun http:acceptor () tbnl:*acceptor*)

#+(or)
(print (cons (find-class 'tbnl-acceptor)
             (sb-mop:class-direct-default-initargs (find-class 'tbnl-acceptor))))

(defmethod initialize-instance ((instance tbnl-acceptor) &rest initargs
                                &key  response-class)
  (declare (dynamic-extent initargs))
  (when response-class
    (setf initargs (copy-list initargs))
    (setf (getf initargs :reply-class) response-class))
  (apply #'call-next-method instance
         initargs))

(defmethod http:start ((acceptor tbnl-acceptor))
  (tbnl:start acceptor))

(defmethod http:acceptor-address ((acceptor tbnl-acceptor))
  (tbnl:acceptor-address acceptor))

(defmethod http:acceptor-request-class ((acceptor tbnl-acceptor))
  'tbnl-request)
(defmethod http:acceptor-response-class ((acceptor tbnl-acceptor))
  'tbnl-response)

(defmethod initialize-instance ((instance tbnl-request) &rest initargs
                                &key method socket headers-in)
  (declare (dynamic-extent initargs))
  (let ((override (rest (assoc :x-http-method-override headers-in))))
    (when (stringp override)
      (setf method
            (or (find-symbol (string-upcase override) http.i::*http-method-package*)
                (http:not-implemented :method override)))))
  (let ((remote-addr nil) (remote-port nil) (local-addr nil) (local-port nil))
    (when socket
      (multiple-value-setq (remote-addr remote-port) (get-peer-address-and-port socket))
      (multiple-value-setq (local-addr local-port) (get-local-address-and-port socket)))
    (apply #'call-next-method instance
           :local-addr local-addr
           :local-port local-port
           :remote-addr remote-addr
           :remote-port remote-port
           :method method
           initargs)))

(defmethod http:acceptor-response-class ((acceptor acceptor))
  (acceptor-reply-class acceptor))

(defmethod http:acceptor-request-class ((acceptor acceptor))
  (acceptor-request-class acceptor))


;;;
;;; request

(defmethod http:request-acceptor ((request tbnl-request))
  (request-acceptor request))

(defmethod http:request-accept-header ((request tbnl-request))
  ;; return the string or nil
  (header-in :accept request))

(defmethod http:request-accept-content-encoding ((request tbnl-request))
  ;; return the string or nil
  (header-in :accept-encoding request))

(defmethod http:request-accept-charset ((request tbnl-request))
  (let ((header (header-in :accept-charset request)))
    (when header
      ;; it is acceptable only iff it is a known keyword.
      ;; otherwise there can be no known encoder
      (find-symbol (string-upcase header) :keyword) (http::not-acceptable))))

(defmethod http:request-argument-list ((request tbnl-request))
  (append (get-parameters request)
          (typecase (http:request-media-type request)
            ((or mime:application/x-www-form-urlencoded mime:multipart/form-data)
             (post-parameters request))
            (t
             nil))))

(defmethod http:request-auth-token ((request request))
  ;; return either the explicit auth-token or the just-user-name authorization header
  (or (get-parameter "auth_token" request)
      (multiple-value-bind (user-name password) (authorization request)
        (when (and user-name (or (null password) (equal password "")))
          user-name))))

(defmethod http:request-authentication ((request request))
  (handler-case (authorization request)
    (error (c)
      (http:bad-request "Invalid authentication: ~a" c))))

(defmethod http:request-content-length ((request tbnl-request))
  (let ((header (header-in :content-length request)))
    (when (plusp (length header))
      (if (every #'digit-char-p header)
        (parse-integer header)
        (http:bad-request)))))

(defmethod http:request-content-stream ((request tbnl-request))
  (content-stream request))

(defmethod (setf http:request-content-stream) (stream (request tbnl-request))
  (setf (slot-value request 'content-stream ) stream))

(defmethod http:request-content-type-header ((request tbnl-request))
  (header-in :content-type request))

(defmethod http:request-header ((request tbnl-request) key)
  (header-in key request))

(defmethod (setf http:request-header) (value (request tbnl-request) key)
  (with-slots (headers-in) request
    (setf headers-in (acons key value (headers-in request)))))

(defmethod (setf http:request-header) ((value null) (request tbnl-request) key)
  (with-slots (headers-in) request
    (setf headers-in (remove (assoc key (headers-in request) :test #'string-equal)
                             headers-in))))

(defmethod http:request-headers ((request tbnl-request))
  (headers-in request))

(defmethod (setf http:request-headers) ((value list) (request tbnl-request))
  (with-slots (headers-in) request
    (setf headers-in value)))

(defmethod http:request-property ((request tbnl-request) key)
  (aux-request-value key request))

(defmethod (setf http:request-property) (value (request tbnl-request) key)
  (setf (aux-request-value key request) value))

#+(or) ;; supposed to be immutable
(defmethod (setf http:request-headers) ((headers list) (request tbnl-request))
  (setf (headers-in request) headers))

(defmethod http:request-host ((request tbnl-request))
  (acceptor-address (request-acceptor request)))

(defmethod http:request-if-match ((request tbnl-request))
  (let ((if-match (header-in :if-match request)))
    (when if-match (split "\\s*,\\s*" if-match))))

(defmethod http:request-if-modified-since ((request tbnl-request))
  (let ((date (header-in :if-modified-since request)))
    (when date
      (http:parse-rfc1123 date))))

(defmethod http:request-keep-alive-p ((request tbnl-request))
  (keep-alive-p request))

(defmethod http:request-origin ((request tbnl-request))
  (header-in :origin request))

(defmethod http:request-original-method ((request tbnl-request))
  (request-method request))

(defmethod http:request-path ((request tbnl-request))
  (script-name request))

(defmethod http:request-post-argument ((request request) key)
  (post-parameter key request))

(defmethod http:request-post-arguments ((request request) key)
  (loop for (name . value) in (post-parameters* request)
        when (equal name key) collect value))

(defmethod http:request-post-argument-list ((request request) &key (methods http::*methods-for-post-parameters*))
  (let ((tbnl:*methods-for-post-parameters* methods))
    (post-parameters* request)))

(defmethod http:request-query-argument ((request request) key)
  (get-parameter key request))

(defmethod http:request-query-arguments ((request request) key)
  (loop for (name . value) in (get-parameters* request)
        when (equal name key) collect value))

(defmethod http:request-query-argument-list ((request request))
  (get-parameters* request))

(defmethod http:request-remote-ip-address ((request request))
  (remote-addr request))

(defmethod http:request-referer ((request request))
  (header-in :referer request))

(defmethod http:request-session-id ((request request))
  (cookie-in (session-cookie-name (request-acceptor request))
             request))

(defmethod http:request-unmodified-since ((request request))
  (let ((date (header-in :unmodified-since request)))
    (when date
      (http:parse-rfc1123 date))))

(defmethod http:request-uri ((request request))
  "the tbnl value is just the path"
  (concatenate 'string "http://" (http:request-host request) (request-uri request)))

(defmethod http:write-request-header ((request request) stream)
  (format stream "~:@(~a~) ~A HTTP/1.1~C~C"
          (request-method request)
          (request-uri request)
          #\Return #\Linefeed)
  (loop for (key . value) in (headers-in request)
    do (write-header-line (chunga:as-capitalized-string key) value stream))
  (format stream "~C~C" #\Return #\Linefeed))


;;;
;;; response

(defmethod http:response-header ((response tbnl-response) (name symbol))
  (header-out name response))

(defmethod (setf http:response-header) ((value null) (response tbnl-response) (name symbol))
  "Given a null header value, delete any header which may be present of the given name"
  (setf (slot-value response 'headers-out)
        (remove name (slot-value response 'headers-out) :key #'first)))

(defmethod (setf http:response-header) ((value string) (response tbnl-response) (name t))
  (setf (header-out name response) value))

(defmethod (setf http:response-header) ((value list) (response tbnl-response) (name t))
  (setf (header-out name response) value))

(defmethod (setf http:response-accept-ranges) ((value string) (response tbnl-response))
  (setf (header-out :accept-ranges response) value))

(defmethod (setf http:response-allow-header) ((allow string) (response tbnl-response))
  (setf (header-out :allow response) allow))

(defmethod (setf http:response-cache-control) ((control string) (response tbnl-response))
  (setf (header-out :cache-control response) control))

(defmethod (setf http:response-character-encoding) (character-encoding (response tbnl-response))
  (setf (header-out :character-encoding response) character-encoding))

(defmethod (setf http:response-content-disposition) ((disposition-type string) (response tbnl-response))
  (setf (header-out :content-disposition response) disposition-type))

(defmethod (setf http:response-content-disposition) ((disposition cons) (response tbnl-response))
  (destructuring-bind (disposition-type . arguments) disposition
    (setf (header-out :content-disposition response) (format nil "~a~{;~a=\"~a\"~}" disposition-type arguments))))

(defmethod (setf http:response-content-encoding) ((coding string) (response tbnl-response))
  (setf (header-out :content-encoding response) coding))

(defmethod (setf http:response-content-encoding) ((coding null) (response tbnl-response))
  (setf (header-out :content-encoding response) coding))

(defmethod (setf http:response-content-length-header) ((value t) (response tbnl-response))
  (setf (header-out :content-length response) value))

(defmethod (setf http:response-content-type-header) ((content-type string) (response tbnl-response))
  (setf (header-out :content-type response) content-type))

(defmethod http:response-content-type-header ((response tbnl-response))
  (header-out :content-type response))

(defmethod (setf http:response-date) ((http-date string) (response tbnl-response))
  (setf (header-out :date response) http-date))

(defmethod (setf http:response-etag) ((tag string) (response tbnl-response))
  (setf (header-out :etag response) tag))

(defmethod (setf http:response-etag) ((tag null) (response tbnl-response))
  (setf (header-out :etag response) tag))

(defmethod (setf http:response-last-modified) ((http-date string) (response tbnl-response))
  (setf (header-out :last-modified response) http-date))

(defmethod (setf http:response-location) (location (response tbnl-response))
  (setf (header-out :location response) location))

(defmethod (setf http:response-www-authenticate-header) (authentication-method (response tbnl-response))
  (setf (header-out :www-authenticate response) authentication-method))

(defmethod (setf http:response-retry-after-header) (time (response tbnl-response))
  (setf (header-out :retry-after response) time))

(defmethod http:response-headers ((response tbnl-response))
  (headers-out response))

(defmethod http:response-status-code ((response tbnl-response))
  (return-code response))

(defmethod (setf http:response-status-code) (code (response tbnl-response))
  (setf (return-code response) code))

(defmethod http:response-transfer-encoding-header ((response tbnl-response))
  (rest (assoc :transfer-encoding (headers-out response))))

(defmethod (setf http:response-transfer-encoding-header) ((mode string) (response tbnl-response))
  (setf (header-out :transfer-encoding response) mode))
(defmethod (setf http:response-transfer-encoding-header) ((mode null) (response tbnl-response))
  (setf (header-out :transfer-encoding response) mode))

(defmethod http:response-vary ((response tbnl-response))
 (header-out :vary response))

(defmethod (setf http:response-vary) (string (response tbnl-response))
  (setf (header-out :vary response) string))


;;;
;;; the native hunchentoot control structure is/was
;;; process-connection
;;; -> get-request-data (including headers)
;;; -> get-post-data (optionally), or otherwise consume request body entity
;;; -> send-headers (possibly implicitly from a call to another operator
;;;    -> send-output
;;;       -> send-response
;;; in which
;;; - the request content stream was created on-the-fly to process post content only
;;; - the resonse status line and the headers are actually emitted by
;;; send-response, with argument presence indicating the intended control-flow.
;;; all, somewhat less than intuitive: it reads as if the invocation through
;;; send-output ues the output stream and sens content or just prepares the
;;; stream, based on content presence while send-headers indicates with null
;;; content, the intent to emit headers only.
;;; - various streams were wrapped around the initial socket stream as required:
;;;  - for request content, one stream for chunking and around that another for decoding
;;;  - for response content, just the chunking stream, with no provision for character output
;;;
;;; rather than depend on argument value side-effects and changes to dynamic bindings,
;;; this control structure
;;; - creates the requisite streams as chunking streams with reader/writer closures
;;; to be used by the stream-* operators for character encoding or by the application
;;; directly for optimization to read/write straight through for non-chunked io. the streams
;;; permit direct specification of character encoding and do not require buffer conversion.
;;; - employs two functions to produce the response
;;;   send-headers
;;;   send-entity-body
;;; to do just that. the application is to invoke them in the correct order, or
;;; leave it to the first reference to the response content stream to trigger
;;; header generation




(defmethod http:respond-to-request ((acceptor acceptor) (request t) (reply t))
  "The default method delegates to the 'Standard implementation for processing a request'
     with the process-request -> handle-request control flow"
  
  (process-request request))


(defmethod process-connection ((acceptor http:acceptor) (socket t))
  "Given acceptor, an http:acceptor, and socket, a connection socket,
 for each request, establish the processing context, in terms of
 *hunchentoot-stream* a possibly chunking duplex stream wrpper for the socket
   for request and response content
 *request* : the reified request instance
 *reply* : the reified response instance
 
 Invoke respond-to-request on the initial acceptor and the request and response instances.
 Upon return ensure the reponse output has completed.
 If the stream is to be kept, iterate with it for the successive requests, otherwise
 close it and return.
 Should a non-local transfer occur, flush and close the stream always."

  (let ((socket-stream (make-socket-stream socket acceptor)))
    (unwind-protect
      ;; process requests until either the acceptor is shut down,
      ;; *CLOSE-HUNCHENTOOT-STREAM* has been set to T by the
      ;; handler, or the peer fails to send a request
      ;; use as the base stream either the original socket stream or, if the connector
      ;; supports ssl, a wrapped stream for ssl support
      (let* ((acceptor-stream (initialize-connection-stream acceptor socket-stream))
             (*hunchentoot-stream* acceptor-stream) ; provide the dynamic binding
             (loop-result nil))
          ;; establish http condition handlers and an error handler which mapps to internal-error
          (handler-bind
            (;; declared conditions are handled according to their report implementation
             (http:condition (lambda (c)
                               (when *reply*  ;; can happen while request is being parsed
                                 (http:send-condition *reply* c)
                                 ;; log the condition as request completion
                                 (acceptor-log-access acceptor :return-code (http:response-status-code *reply*)))
                               (when (typep c 'http:error) ;; ensure syslog
                                 (http:log-error "process-connection: condition signaled in http response: [~a][~a] ~a "
                                                 (type-of c)
                                                 *request*
                                                 c))
                               ;;(describe *reply*)
                               ;;(describe (http:response-content-stream *reply*))
                               ;;(dotimes (x 100) (write-char #\. (http:response-content-stream *reply*)))
                               ;;(finish-output (http:response-content-stream *reply*))
                               ;;(dotimes (x 100) (write-byte (char-code #\,) acceptor-stream))
                               ;;(finish-output acceptor-stream)
                               ;; (format *trace-output*  "sent~%~a~%" c)
                               (return-from process-connection
                                 (values nil c nil)))))
            (handler-bind
              ;; establish an additional level to permit a general error handler which maps to http:condition
              (;; at this level decline to handle http:condition, to cause it to pass one level up
               (http:condition (lambda (c)
                                 (signal c)))
               ;; a connection error is reported to the application, but subsequently suppressed by returning from the connection handler.
               ;; this does not try to continue as any stream's socket
               (usocket:connection-aborted-error (lambda (c)
                                                   (http:handle-condition acceptor c)
                                                   (http:log-error "process-connection: [~a] ~a" (type-of c) c)
                                                   (return-from process-connection nil)))
               #+sbcl
               ;; caused by a broken pipe
               ;; handled in the same way as socket errors
               (sb-int:simple-stream-error (lambda (c)
                                             (http:handle-condition acceptor c)
                                             (http:log-error "process-connection: [~a] ~a" (type-of c) c)
                                             (return-from process-connection nil)))
               (bt:timeout (lambda (c)
                             (declare (ignore c))
                             (http:log-notice "process-connection: request data timed out")
                             (http:request-timeout )))
               ;; while any other error is handled as per acceptor, where the default implementation
               ;; will be to log and re-signal as an http:internal-error, but other mapping are possible
               ;; as well as declining to handle in which the condition is re-signaled as an internal error
               (error (lambda (c)
                        (http:handle-condition acceptor c)
                        ;; if it remains unhandled, then resignal as an internal error
                        (http:log-error "process-connection: unhandled error in http response: [~a] ~a" (type-of c) c)
                        (format *error-output* "process-connection: unhandled error in http response: [~a] ~a" (type-of c) c)
                        (format *error-output* "~%~a" (get-backtrace))
                        ;; re-signal to the acceptor's general handler
                        (http:internal-error "process-connection: unhandled error in http response: [~a] ~a" (type-of c) c))))
            
              (setf loop-result
              (catch 'hunchentoot::handler-done 
                    ;; something or other in hunchentoot code decided to give up
                    ;; maybe while constructing the request, maybe when actually done.
                    ;;
                  (loop
                (let ((*close-hunchentoot-stream* t))
                  (when (acceptor-shutdown-p acceptor)
                    (return))
                  (multiple-value-bind (headers-in method url-string protocol)
                                       (handler-case (get-request-data-with-timeout *hunchentoot-stream*)
                                         (error (c)
                                           ;; if the request is unreadable, respond and return
                                           (format acceptor-stream "HTTP/1.1 400 ~A~C~C~C~C~A~C~C"
                                                   (reason-phrase 400)
                                                   #\Return #\Linefeed  #\Return #\Linefeed
                                                   c
                                                    #\Return #\Linefeed)
                                           (finish-output acceptor-stream)
                                           (return-from process-connection
                                             (values nil c nil))))
                    ;; check if there was a request at all
                    (unless method
                      (return))
                    ;; bind per-request special variables, then process the
                    ;; request - note that *ACCEPTOR* was bound by an aound method
                    (let* ((output-stream (make-instance 'http:output-stream :real-stream *hunchentoot-stream*))
                           (*reply* (http:make-response acceptor
                                                        :server-protocol protocol
                                                        ;; create the output stream which supports character output for the headers
                                                        ;; with the initial character encoding set to ascii
                                                        :content-stream output-stream))
                           (input-stream (make-instance 'http:input-stream :real-stream *hunchentoot-stream*))
                           (*request* (http:make-request acceptor
                                                         :socket socket
                                                         :headers-in headers-in
                                                         :content-stream input-stream
                                                         :method method
                                                         :uri url-string
                                                         :server-protocol protocol))
                           (http:*request* *request*)
                           (http:*response* *reply*)
                           (*tmp-files* nil)
                           (*session* nil)
                           (transfer-encodings (cdr (assoc* :transfer-encoding headers-in))))
                      ;; instantiation must follow this order as any errors are recorded as side-effects on the response
                      ;; return code, which must be checked...
                      (setf (http:response-request *reply*) *request*)
                      (setf (http:request-response *request*) *reply*)
                      (when transfer-encodings
                        (setq transfer-encodings
                              (split "\\s*,\\s*" transfer-encodings))
                        (when (member "chunked" transfer-encodings :test #'equalp)
                          (cond ((acceptor-input-chunking-p acceptor)
                                 ;; turn chunking on before we read the request body
                                 (setf (chunked-stream-input-chunking-p input-stream) t))
                                (t (http:bad-request "Client tried to use chunked encoding, but acceptor is configured to not use it.")))))
                      (if (eql +http-ok+ (return-code *reply*))
                          ;; if initialization succeeded, process
                          (with-acceptor-request-count-incremented (acceptor)
                            ;; at this point thread counts as active wrt eventual soft shutdown (see stop)
                            (catch 'request-processed
                              (http:respond-to-request acceptor *request* *reply*)))
                          ;; otherwise, report the error
                          (http:error :code (return-code *reply*)))
                      ;; iff chunking, emit the last chunk and then the terminating chunk
                      (force-output output-stream)
                      (when (chunga:chunked-stream-output-chunking-p output-stream)
                        (setf (chunga:chunked-stream-output-chunking-p output-stream) nil))
                      ;;(reset-connection-stream *acceptor* (http:response-content-stream *reply*))
                      ;; access log message
                      (acceptor-log-access acceptor :return-code (http:response-status-code *reply*)))
                    ;; synchronize on the underlying stream
                    (finish-output acceptor-stream)
                    (when *close-hunchentoot-stream*
                      (return)))))
                    socket-stream))
              (unless (eq socket-stream loop-result)
                ;; something was thrown
                (http:log-warn "process-connection: thrown out of handler: ~s" loop-result)
                ;; cannot. there is no reply bound (http:bad-request "Unspecified issue.")
                (finish-output acceptor-stream))))
        (close acceptor-stream :abort t)
        (setq socket-stream nil))
      (when socket-stream
        ;; as we are at the end of the request here, we ignore all
        ;; errors that may occur while flushing and/or closing the
        ;; stream.
        ;; as the socket stream is still bound, an error occurred - do not flush, just close
        (ignore-errors*
         (close socket-stream :abort t))))))


(defmethod http:log (level (destination http:acceptor) format-control &rest arguments)
  (declare (dynamic-extent arguments))
  (with-log-stream (stream (acceptor-message-log-destination destination) *message-log-lock*)
    (apply #'http:log level stream format-control arguments)))

(defmethod acceptor-log-access ((acceptor tbnl-acceptor) &key return-code)
  "Replace the default method for access logging.
 Retain the default format, but do not emeit literal authorization and allow for nginx alternatives
 for the true ip address."
  (http:log-notice "~:[-~@[ (~A)~]~;~:*~A~@[ (~A)~]~] ~A [~A] \"~A ~A~@[?~A~] ~
                    ~A\" ~D ~:[-~;~:*~D~] \"~:[-~;~:*~A~]\" \"~:[-~;~:*~A~]\"~%"
            (remote-addr*)
            (or (header-in* :x-forwarded-for)
                (header-in* :X-Real-IP))
            (not (null (authorization)))
            (iso-time)
            (request-method*)
            (script-name*)
            (query-string*)
            (server-protocol*)
            return-code
            (content-length*)
            (referer)
            (user-agent)))

(defmethod http:send-headers ((response tbnl-response))
  "Buffer headers to the response stream.
 Returns the response stream."
  
  (let* ((content-stream (http::get-response-content-stream response))
         (client-header-stream (http:stream-header-stream content-stream))
         (header-stream (if *header-stream*
                            (make-broadcast-stream *header-stream* client-header-stream)
                            client-header-stream))
         (headers-out (headers-out response))
         (content-length (rest (assoc :content-length headers-out)))
         (head-request-p (eq :head (request-method (http:response-request response))))
         (server nil)
         (date nil)
         (status-code (http:response-status-code response))
         (chunked-p (and (acceptor-output-chunking-p (http:response-acceptor response))
                         (eq (http:response-protocol response) :http/1.1)
                         ;; only turn chunking on if the content
                         ;; length is unknown at this point...
                         (null content-length)
                         (not (eql status-code 204)))))
    ;; emit the response and entity headers
    ;; start with status line
    (format header-stream "HTTP/1.1 ~D ~A~C~C" status-code (reason-phrase status-code) #\Return #\Linefeed)
    ;; write all headers from the REPLY object
    (when chunked-p
      (setf headers-out (acons :transfer-encoding "chunked" headers-out)))
    (loop for (key . value) in headers-out
          when value
          do (case key
               (:date (setf date t))
               (:server (setf server t)))
          and do (if (consp value)
                     (loop for value in value
                       do (write-header-line (as-capitalized-string key) value header-stream))
                     (write-header-line (as-capitalized-string key) value header-stream)))
    (unless date
      (setf date (rfc-1123-date))
      (write-header-line (as-capitalized-string :date) date header-stream)
      (setf headers-out (acons :date date headers-out)))
    (unless server
      (setf server (acceptor-server-name (http:response-acceptor response)))
      (write-header-line (as-capitalized-string :server) server header-stream)
      (setf headers-out (acons :server server headers-out)))
    ;; the slot definition includes a reader only
    (setf (slot-value response 'headers-out) headers-out)
    
    (multiple-value-bind (keep-alive-p keep-alive-requested-p)
                         (http:response-keep-alive-p response)
      (when keep-alive-p
        (setq keep-alive-p
              ;; use keep-alive if there's a way for the client to
              ;; determine when all content is sent (or if there
              ;; is no content)
              (or chunked-p
                  head-request-p
                  (eql (return-code*) +http-not-modified+)
                  content-length)))
      
      ;; now emit keep-alive headers
      (cond (keep-alive-p
             (setf *close-hunchentoot-stream* nil)
             (when (and (acceptor-read-timeout (http:response-acceptor response))
                        (or (not (eq (http:response-protocol response) :http/1.1))
                            keep-alive-requested-p))
               ;; persistent connections are implicitly assumed for
               ;; HTTP/1.1, but we return a 'Keep-Alive' header if the
               ;; client has explicitly asked for one
               (write-header-line (as-capitalized-string :connection) "Keep-Alive" header-stream)
               (write-header-line (as-capitalized-string :keep-alive)
                                  (format nil "timeout=~D" (acceptor-read-timeout (http:response-acceptor response)))
                                  header-stream)))
            (t
             (write-header-line (as-capitalized-string :connection) "Close" header-stream)
             ))
      (setf (http:response-close-stream-p response) keep-alive-p))
    
    ;; now the cookies
    (loop for (nil . cookie) in (cookies-out response)
          do (write-header-line "Set-Cookie" (stringify-cookie cookie) header-stream))
    ;; now the terminating EOL
    (format header-stream "~C~C" #\Return #\Linefeed)
    ;; Read post data to clear stream - Force binary mode to avoid OCTETS-TO-STRING overhead.
    ;; this is transcribed from the original hunchentoot implementation, but seems bogus
    ;; one could check for eof, but that would preclude pipelined interaction
    ;; (raw-post-data :force-binary t)
    content-stream))

;;; support for asynchronous operations

(defun is-http-uri (object)
  (let ((scanner (load-time-value (cl-ppcre:create-scanner `(:sequence :start-anchor
                                       (:alternation "http" "https")
                                       "://"
                                       (:register
                                        (:greedy-repetition 0 nil (:inverted-char-class #\/)))
                                       (:GREEDY-REPETITION 0 NIL :EVERYTHING)
                                       :end-anchor)))))
    (and (stringp object) (cl-ppcre:scan scanner object))))

(defgeneric call-with-open-response-stream (operator location &rest args)
  (:method (operator (location pathname) &rest args)
    (declare (dynamic-extent args)
             (dynamic-extent operator))
    (flet ((plist-difference (plist keys)
             (loop for (key value) on plist by #'cddr
               unless (member key keys)
               collect key and
               collect value)))
      (setf args (plist-difference args '(:content-type :method))))
    (let ((stream nil) (abort t))
      (ensure-directories-exist location)
      (unwind-protect (progn (setf stream (apply #'open location
                                                 :direction :output
                                                 :if-exists :error
                                                 :if-does-not-exist :create
                                                 :element-type :default
                                                 args))
                        (funcall operator stream)
                        (setf abort nil))
        (when stream (close stream :abort abort)))))
  (:method (operator (location puri:uri) &rest args)
    (declare (dynamic-extent args)
             (dynamic-extent operator))
    (let ((response (apply #'drakma:http-request location :content operator :protocol :HTTP/1.1
                           :want-stream t args)))
      (when (and (streamp response) (open-stream-p response))
        (close response))))
  (:method (operator (location string) &rest args)
    (declare (dynamic-extent args)
             (dynamic-extent operator))
    (if (is-http-uri location)
        (apply #'call-with-open-response-stream operator (puri:uri location) args)
        (apply #'call-with-open-response-stream operator (pathname location) args))))

(defgeneric call-with-open-request-stream (operator location &rest args)
  (:method (operator (location pathname) &rest args)
    (declare (dynamic-extent args)
             (dynamic-extent operator))
    (flet ((plist-difference (plist keys)
             (loop for (key value) on plist by #'cddr
               unless (member key keys)
               collect key and
               collect value)))
      (setf args (plist-difference args '(:accept :method))))
    (let ((stream nil) (abort t))
      (ensure-directories-exist location)
      (unwind-protect (progn (setf stream (apply #'open location
                                                 :direction :input
                                                 :if-does-not-exist :error
                                                 :element-type :default
                                                 args))
                        (funcall operator stream)
                        (setf abort nil))
        (when stream (close stream :abort abort)))))
  (:method (operator (location puri:uri) &rest args &key (method :get) &allow-other-keys)
    (declare (dynamic-extent args)
             (dynamic-extent operator))
    (apply #'drakma:http-request location :content operator :method method
           :protocol :HTTP/1.1
           args))
  (:method (operator (location string) &rest args)
    (declare (dynamic-extent args)
             (dynamic-extent operator))
    (if (is-http-uri location)
        (apply #'call-with-open-request-stream operator (puri:uri location) args)
        (apply #'call-with-open-request-stream operator (pathname location) args))))
  

(defmacro with-open-response-stream ((stream-var location &rest args) &body body)
  (let ((op (gensym "CWORS-")))
  `(flet ((,op (,stream-var)
            ,@body))
     (declare (dynamic-extent #',op))
     (call-with-open-response-stream #',op ,location ,@args))))

(defmacro with-open-request-stream ((stream-var location &rest args) &body body)
  (let ((op (gensym "CWORS-")))
  `(flet ((,op (,stream-var)
            ,@body))
     (declare (dynamic-extent #',op))
     (call-with-open-request-stream #',op ,location ,@args))))

(defparameter *request-data-timeout* 10)

(defun get-request-data-no-continue (stream)
  "Reads incoming headers as get-request-data, but ignore continue indications."
  (bt:with-timeout (*request-data-timeout*)
    (with-character-stream-semantics
        (let ((first-line (read-initial-request-line stream)))
          (when first-line
            (assert (every #'printable-ascii-char-p first-line) ()
                    "Non-ASCII character in request line ~A" stream)
            (destructuring-bind (&optional method url-string protocol)
                                (split "\\s+" first-line :limit 3)
              (assert url-string ()
                      "No url in request line ~A" first-line)
              (when *header-stream*
                (format *header-stream* "~A~%" first-line))
              (let ((headers (and protocol (read-http-headers stream *header-stream*))))
                (unless protocol (setq protocol "HTTP/0.9"))
                (values headers
                        (as-keyword method)
                        url-string
                        (as-keyword (trim-whitespace protocol))))))))))

(defun get-request-data-with-timeout (stream)
  (bt:with-timeout (*request-data-timeout*)
    (get-request-data stream)))

(defgeneric process-asynchronous-connection (acceptor source destination &key if-exists transfer-encoding)
  (:documentation "Given acceptor, an http:acceptor, and connenction which is split
 between a source and a destination, establish the processing context, in terms of
 simplex streams
 - a non-chunked input stream
 - an output stream, opened to the destination chunked according to protocol
   for request and response content
 *request* : the reified request instance
 *reply* : the reified response instance
 
 Perform respond-to-request w/ error handling as for a synchronous request.
 Methods are specialized for combinations of source and destination streams and abstract locations.
 In order for queries to succeed, the original request must include a content-length header,
 in order that it be available to decode the cached request stream.")

  (:method ((acceptor http:acceptor) (source pathname) (destination t) &rest args)
    (with-open-file (source-stream source :direction :input 
                                   :element-type :default)
      (apply #'process-asynchronous-connection acceptor source-stream destination args)))

  (:method ((acceptor http:acceptor) (source t) (destination pathname) &rest args &key (if-exists :error) &allow-other-keys)
    (with-open-file (destination-stream destination :direction :output 
                                        :element-type :default
                                        :if-does-not-exist :create
                                        :if-exists if-exists)
      (apply #'process-asynchronous-connection acceptor source destination-stream args)))

  (:method ((acceptor http:acceptor) (*hunchentoot-stream* stream) (response-stream stream) &key transfer-encoding if-exists
            &aux request reply)
    "process a single asynchronous request using the base stream as-is"
    (declare (ignore if-exists))
    (unwind-protect
        ;; establish http condition handlers and an error handler which mapps to internal-error
        (handler-bind
            ((http:condition (lambda (c)
                               ;; declared conditions are handled according to their report implementation
                               (when reply  ;; error can occurr before the reply is read
                                 (http:send-condition reply c)
                                 ;; log the condition as request completion
                                 (acceptor-log-access acceptor :return-code (http:response-status-code reply)))
                               (when (typep c 'http:error) ;; should not be
                                 (http:log-error "process-asynchronous-connection: condition signaled in http response: [~a] ~a" (type-of c) c))
                               (return-from process-asynchronous-connection
                                 (values nil c nil))))
             ;; if some other error reaches here, log and ignore it.
             (error (lambda (c)
                      (http:log-error "process-asynchronous-connection: error in http response: [~a] ~a" (type-of c) c)
                      (return-from process-asynchronous-connection
                        (values nil c nil)))))
          (handler-bind
              ;; establish an additional level to permit a general handler which maps to http:condition
              (;; at this level decline to handle http:condition, to cause it to pass one level up
               (http:condition (lambda (c)
                                 (signal c)))
               ;; while any other error is handled as per acceptor, where the default implementation
               ;; will be to log and re-signal as an http:internal-error, but other mapping are possible
               ;; as well as declining to handle in which the condition is re-signaled as an internal error
               (bt:timeout (lambda (c)
                             (http:request-timeout )))
               (error (lambda (c)
                        (http:handle-condition acceptor c)
                        ;; if it remains unhandled, then resignal as an internal error
                        (http:log-error "process-asynchronous-connection: unhandled error in http response: [~a] ~a" (type-of c) c)
                        (format *error-output* "process-asynchronous-connection: unhandled error in http response: [~a] ~a" (type-of c) c)
                        (format *error-output* "~%~a" (get-backtrace))
                        ;; re-signal to the acceptor's general handler
                        (http:internal-error "process-asynchronous-connection: unhandled error in http response: [~a] ~a" (type-of c) c))))
            
              (multiple-value-bind (headers-in method url-string protocol)
                                   (get-request-data-no-continue *hunchentoot-stream*)
                ;; check if there was a request at all
                (if method
                    ;; bind per-request special variables, then process the
                    ;; request - note that *ACCEPTOR* was bound by an aound method
                    (let* ((*acceptor* acceptor)
                           (output-stream (make-instance 'http:output-stream :real-stream response-stream))
                           (*reply* (http::make-response acceptor
                                                         :server-protocol :HTTP/1.0 ; protocol
                                                         ;; create the output stream which supports character output for the headers
                                                         ;; with the initial character encoding set to ascii
                                                         :content-stream output-stream))
                           (input-stream (make-instance 'http:input-stream :real-stream *hunchentoot-stream*))
                           (*request* (http:make-request acceptor
                                                         :headers-in headers-in
                                                         :content-stream input-stream
                                                         :method method
                                                         :uri url-string
                                                         :server-protocol protocol))
                           (http:*request* *request*)
                           (http:*response* *reply*)
                           (*tmp-files* nil)
                           (*session* nil)
                           (transfer-encodings (cdr (assoc* :transfer-encoding headers-in))))
                      ;; instantiation must follow this order as any errors are recorded as side-effects on the response
                      ;; return code, which must be checked...
                      (setf request *request*)
                      (setf reply *reply*)
                      (setf (http:response-request *reply*) *request*)
                      (setf (http:request-response *request*) *reply*)
                      (when transfer-encodings
                        (setq transfer-encodings
                              (split "\\s*,\\s*" transfer-encodings))
                        (when (member "chunked" transfer-encodings :test #'equalp)
                          (cond ((acceptor-input-chunking-p acceptor)
                                 ;; turn chunking on before we read the request body
                                 (setf (chunked-stream-input-chunking-p input-stream) t))
                                (t (http:bad-request "Client tried to use chunked encoding, but acceptor is configured not to use it.")))))
                      (case transfer-encoding
                        (:chunked
                         (setf (chunked-stream-output-chunking-p output-stream) t)
                         (setf (http:response-transfer-encoding-header *reply*) "chunked"))
                        (t
                         (setf (chunked-stream-output-chunking-p output-stream) nil)
                         (setf (http:response-transfer-encoding-header *reply*) nil)))
                      (if (eql +http-ok+ (return-code *reply*))
                          ;; if initialization succeeded, process
                          ;; at this point thread counts as active wrt eventual soft shutdown (see stop)
                          (catch 'request-processed
                            (http:respond-to-request acceptor *request* *reply*))
                          ;; otherwise, report the error
                          (http:error :code (return-code *reply*)))
                      (finish-output output-stream)
                      ;;(reset-connection-stream *acceptor* (http:response-content-stream *reply*))
                      ;; access log message
                      (acceptor-log-access acceptor :return-code (http:response-status-code *reply*))
                      (finish-output output-stream)
                      (values *request* *reply*))
                    (http:log-error "process-asynchronous-connection: invalid request data: ~s ~s ~s ~s"
                                    headers-in method url-string protocol))))))))

#+(or) ;;; does not work to emit the reponse as a request
(defgeneric process-asynchronous-connection (acceptor source destination &key output)
  (:documentation "Given acceptor, an http:acceptor, and connenction which is split
 between a source and a destination, establish the processing context, in terms of
 simplex streams
 - a non-chunked input stream
 - an output stream, opened to the destination chunked according to protocol
   for request and response content
 *request* : the reified request instance
 *reply* : the reified response instance
 
 Perform respond-to-request w/ error handling as for a synchronous request.
 Methods are specialized for combinations of source and destination streams and abstract locations")

  (:method ((acceptor http:acceptor) (source pathname) (destination t) &rest args)
    (with-open-file (source-stream source :direction :input 
                                   :element-type :default)
      (apply #'process-asynchronous-connection acceptor source-stream destinationargs)))

  (:method ((acceptor http:acceptor) (source t) (destination pathname) &rest args)
    (with-open-file (destination-stream source :direction :output 
                                   :element-type :default)
      (apply #'process-asynchronous-connection acceptor source destination-stream args)))

  (:method ((acceptor http:acceptor) (*hunchentoot-stream* stream) (response-stream stream) &key output &aux request reply)
    "process a single asynchronous request using the base stream as-is"
    (unwind-protect
        ;; establish http condition handlers and an error handler which mapps to internal-error
        (handler-bind
            ((http:condition (lambda (c)
                               ;; declared conditions are handled according to their report implementation
                               (http:send-condition reply c)
                               ;; log the condition as request completion
                               (acceptor-log-access acceptor :return-code (http:response-status-code *reply*))
                               (when (typep c 'http:error)
                                 (http:log-error "process-asynchronous-connection: condition signaled in http response: [~a] ~a" (type-of c) c))
                               (return-from process-asynchronous-connection
                                 (values nil c nil))))
             ;; if some other error reaches here, log and ignore it.
             (error (lambda (c)
                      (http:log-error "process-asynchronous-connection: error in http response: [~a] ~a" (type-of c) c)
                      (return-from process-asynchronous-connection
                        (values nil c nil)))))
          (handler-bind
              ;; establish an additional level to permit a general handler which maps to http:condition
              (;; at this level decline to handle http:condition, to cause it to pass one level up
               (http:condition (lambda (c)
                                 (signal c)))
               ;; while any other error is handled as per acceptor, where the default implementation
               ;; will be to log and re-signal as an http:internal-error, but other mapping are possible
               ;; as well as declining to handle in which the condition is re-signaled as an internal error
               (error (lambda (c)
                        (http:handle-condition acceptor c)
                        ;; if it remains unhandled, then resignal as an internal error
                        (http:log-error acceptor "process-connection: unhandled error in http response: [~a] ~a" (type-of c) c)
                        (format *error-output* "process-connection: unhandled error in http response: [~a] ~a" (type-of c) c)
                        (format *error-output* "~%~a" (get-backtrace))
                        ;; re-signal to the acceptor's general handler
                        (http:internal-error "process-connection: unhandled error in http response: [~a] ~a" (type-of c) c))))
            
              (multiple-value-bind (headers-in method url-string protocol)
                                   (get-request-data *hunchentoot-stream*)
                ;; check if there was a request at all
                (if method
                    (let ((asynchronous-end-point (or output
                                                      (http:request-header headers-in "Asynchronous-Location")
                                                      (http:request-header headers-in "Asynchronous-End-Point")))
                          (asynchronous-method (intern (string (or (http:request-header headers-in "Asynchronous-Method") :post)) :keyword))
                          (asynchronous-content-type (http:request-header headers-in "Asynchronous-Content-Type")))
                      ;; although this serves as the internal response stream, it must appears to the
                      ;; remote location as a request
                      (with-open-request-stream (response-stream asynchronous-end-point
                                                                 :method asynchronous-method
                                                                 :content-type asynchronous-content-type)
                        ;; bind per-request special variables, then process the
                        ;; request - note that *ACCEPTOR* was bound by an aound method
                        (let* ((*acceptor* acceptor)
                               (output-stream (make-instance 'http:output-stream :real-stream response-stream))
                               (*reply* (http::make-response acceptor
                                                             ;; create the output stream which supports character output for the headers
                                                             ;; with the initial character encoding set to ascii
                                                             :content-stream output-stream
                                                             :method asynchronous-method))
                               (input-stream (make-instance 'http:input-stream :real-stream *hunchentoot-stream*))
                               (*request* (http:make-request acceptor
                                                             :headers-in headers-in
                                                             :content-stream input-stream
                                                             :method method
                                                             :uri url-string
                                                             :server-protocol protocol))
                               (http:*request* *request*)
                               (http:*response* *reply*)
                               (*tmp-files* nil)
                               (*session* nil)
                               (transfer-encodings (cdr (assoc* :transfer-encoding headers-in))))
                          ;; instantiation must follow this order as any errors are recorded as side-effects on the response
                          ;; return code, which must be checked...
                          (setf request *request*)
                          (setf reply *reply*)
                          (setf (http:response-request *reply*) *request*)
                          (setf (http:request-response *request*) *reply*)
                          (when transfer-encodings
                            ;; ignore it
                            )
                          (if (eql +http-ok+ (return-code *reply*))
                              ;; if initialization succeeded, process
                              ;; at this point thread counts as active wrt eventual soft shutdown (see stop)
                              (catch 'request-processed
                                (http:respond-to-request acceptor *request* *reply*))
                              ;; otherwise, report the error
                              (http:error :code (return-code *reply*)))
                          (finish-output output-stream)
                          ;;(reset-connection-stream *acceptor* (http:response-content-stream *reply*))
                          ;; access log message
                          (acceptor-log-access acceptor :return-code (http:response-status-code *reply*))
                          (finish-output output-stream)
                          (values *request* *reply*))))
                    (http:log-error "process-asynchronous-connection: invalid request data: ~s ~s ~s ~s"
                              headers-in method url-string protocol))))))))

