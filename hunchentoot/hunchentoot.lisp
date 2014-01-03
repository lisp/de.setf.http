;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: HUNCHENTOOT; -*-
;;; Copyright (c) 2004-2010, Dr. Edmund Weitz.  All rights reserved.
;;; Copyright 2013 [james anderson](mailto:james.anderson@setf.de) All Rights Reserved

(in-package :hunchentoot)


;;; hunchentoot adaptations and extensions to integrate generic request management


(defclass tbnl-request (http:request request)
  ())

(defclass tbnl-response (http:response reply)
  ())

(defclass tbnl-acceptor (http:acceptor acceptor)
  ((reply-class :initarg :response-class))
  (:default-initargs
    :request-class 'tbnl-request
    :response-class 'tbnl-response))

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

(defmethod http:acceptor-request-class ((acceptor tbnl-acceptor))
  'tbnl-request)
(defmethod http:acceptor-response-class ((acceptor tbnl-acceptor))
  'tbnl-response)

(defmethod initialize-instance ((instance tbnl-request) &rest initargs
                                &key socket)
  (declare (dynamic-extent initargs))
  (multiple-value-bind (remote-addr remote-port)
                       (get-peer-address-and-port socket)
    (multiple-value-bind (local-addr local-port)
                         (get-local-address-and-port socket)
      (apply #'call-next-method instance
             :local-addr local-addr
             :local-port local-port
             :remote-addr remote-addr
             :remote-port remote-port
             initargs))))

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
  (append (get-parameters request) (post-parameters request)))

(defmethod http:request-auth-token ((request request))
  (or (header-in :authorization request)
      (get-parameter "auth_token" request)))

(defmethod http:request-authentication ((request request))
  (authorization request))

(defmethod http:request-content-length ((request tbnl-request))
  (let ((header (header-in :content-length request)))
    (when (plusp (length header))
      (if (every #'digit-char-p header)
        (parse-integer header)
        (http:bad-request)))))

(defmethod http:request-content-stream ((request tbnl-request))
  (content-stream request))

(defmethod http:request-content-type-header ((request tbnl-request))
  (header-in :content-type request))

(defmethod http:request-host ((request tbnl-request))
  (acceptor-address (request-acceptor request)))

(defmethod http:request-if-modified-since ((request request))
  (let ((date (header-in :if-modified-since request)))
    (when date
      (http:parse-rfc1123 date))))

(defmethod http:request-original-method ((request tbnl-request))
  (request-method request))

(defmethod http:request-path ((request tbnl-request))
  (script-name request))

(defmethod http:request-post-argument ((request request) key)
  (post-parameter key request))

(defmethod http:request-post-arguments ((request request) key)
  (loop for (name . value) in (post-parameters* request)
        when (equal name key) collect value))

(defmethod http:request-query-argument ((request request) key)
  (get-parameter key request))

(defmethod http:request-query-arguments ((request request) key)
  (loop for (name . value) in (get-parameters* request)
        when (equal name key) collect value))

(defmethod http:request-remote-ip-address ((request request))
  (remote-addr request))

(defmethod http:request-session-id ((request request))
  (cookie-in (session-cookie-name (request-acceptor request))
             request))

(defmethod http:request-unmodified-since ((request request))
  (let ((date (header-in :unmodified-since request)))
    (when date
      (http:parse-rfc1123 date))))

;;;
;;; response

(defmethod (setf http:response-accept-ranges) ((value string) (response tbnl-response))
  (setf (header-out :accept-ranges response) value))

(defmethod (setf http:response-accept-encoding) ((value string) (response tbnl-response))
  (setf (header-out :accept-encoding response) value))

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

(defmethod http:response-status-code ((response tbnl-response))
  (return-code response))

(defmethod (setf http:response-status-code) (code (response tbnl-response))
  (setf (return-code response) code))

(defmethod http:response-transfer-encoding-header ((response tbnl-response))
  (rest (assoc :transfer-encoding (headers-out response))))

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
  "GIven acceptor, an http:acceptor, and socket, a connection socket,
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
        (let ((*hunchentoot-stream* (initialize-connection-stream acceptor socket-stream)))
          ;; establish http condition handlers and an error handler which mapps to internal-error
          (handler-bind
            ((http:condition (lambda (c)
                               ;; when the headers are still pending, emit an error report as the
                               ;; response. otherwise, just terminate the processing
                               (when (http:response-headers-unsent-p *reply*)
                                 (http:report-condition-headers c *reply*)
                                 (http:send-headers *reply*))
                               ;; log the condition as request completion
                               (acceptor-log-access acceptor :return-code (http:response-status-code *reply*))
                               (return-from process-connection
                                 (values nil c nil))))
             (error (lambda (c)
                      (http:log *lisp-errors-log-level* acceptor "unhandled error in http response: ~a" c)
                      (format *error-output* "~%~a" (get-backtrace))
                      ;; re-signal to the acceptor's general handler
                      (http:internal-error :message (format nil "unhandled error in http response: ~a" c)))))
            
            (loop
              (let ((*close-hunchentoot-stream* t))
                (when (acceptor-shutdown-p acceptor)
                  (return))
                (multiple-value-bind (headers-in method url-string protocol)
                                     (get-request-data *hunchentoot-stream*)
                  ;; check if there was a request at all
                  (unless method
                    (return))
                  ;; bind per-request special variables, then process the
                  ;; request - note that *ACCEPTOR* was bound by an aound method
                  (let ((transfer-encodings (cdr (assoc* :transfer-encoding headers-in))))
                    (when transfer-encodings
                      (setq transfer-encodings
                            (split "\\s*,\\s*" transfer-encodings))
                      (when (member "chunked" transfer-encodings :test #'equalp)
                        (cond ((acceptor-input-chunking-p acceptor)
                               ;; turn chunking on before we read the request body
                               (setf *hunchentoot-stream* (make-chunked-stream *hunchentoot-stream*)
                                     (chunked-stream-input-chunking-p *hunchentoot-stream*) t))
                              (t (hunchentoot-error "Client tried to use chunked encoding, but acceptor is configured to not use it."))))))
                  (let* ((*request* (http:make-request acceptor
                                                       :socket socket
                                                       :headers-in headers-in
                                                       :content-stream *hunchentoot-stream*
                                                       :method method
                                                       :uri url-string
                                                       :server-protocol protocol))
                         (*reply* (http:make-response acceptor
                                                      :request *request*
                                                      :keep-alive-p (keep-alive-p *request*)
                                                      :server-protocol protocol
                                                      ;; create the output stream which supports character output for the headers
                                                      ;; with the initial character encoding set to ascii
                                                      :content-stream (make-instance 'http:output-stream :real-stream socket-stream)))
                         (http:*request* *request*)
                         (http:*response* *reply*)
                         (*tmp-files* nil)
                         (*session* nil))
                    (with-acceptor-request-count-incremented (acceptor)
                      (catch 'request-processed
                        (http:respond-to-request acceptor *request* *reply*)))
                    (finish-output (http:response-content-stream *reply*))
                    ;;(reset-connection-stream *acceptor* (http:response-content-stream *reply*))
                    ;; access log message
                    (acceptor-log-access acceptor :return-code (http:response-status-code *reply*)))
                  ;;(finish-output *hunchentoot-stream*)
                  
                  (setq *hunchentoot-stream* (reset-connection-stream *acceptor* *hunchentoot-stream*))
                  (finish-output socket-stream)
                  (when *close-hunchentoot-stream*
                    (return)))))))
      (progn
        ;; as we are at the end of the request here, we ignore all
        ;; errors that may occur while flushing and/or closing the
        ;; stream.
        (ignore-errors*
          (finish-output socket-stream))
        (ignore-errors*
          (close socket-stream :abort t))))))


(defmethod http:log (level (destination http:acceptor) format-control &rest arguments)
  (declare (dynamic-extent arguments))
  (apply #'acceptor-log-message destination level format-control arguments))



(defmethod http:send-headers ((response tbnl-response))
  "Sends all headers and maybe the content body to
*HUNCHENTOOT-STREAM*.  Returns immediately and does nothing if called
more than once per request.  Called by PROCESS-REQUEST and/or
SEND-HEADERS.  The RETURN-CODE argument represents the integer return
code of the request.  The corresponding reason phrase is determined by
calling the REASON-PHRASE function.  The CONTENT provided represents
the body data to send to the client, if any.  If it is not specified,
no body is written to the client.  The handler function is expected to
directly write to the stream in this case.

Returns the stream that is connected to the client."
  
  (let* ((header-stream (http:response-content-stream response))
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
          and do (write-header-line (as-capitalized-string key) value header-stream))
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
             (write-header-line (as-capitalized-string :connection) "Close" header-stream)))
      (setf (http:response-close-stream-p response) keep-alive-p))
    
    ;; now the cookies
    (loop for (nil . cookie) in (cookies-out response)
          do (write-header-line "Set-Cookie" (stringify-cookie cookie) header-stream))
    (format header-stream "~C~C" #\Return #\Linefeed)
    
    ;; Read post data to clear stream - Force binary mode to avoid OCTETS-TO-STRING overhead.
    ;; this is transacibed from the original hunchentoot implementation, but seems bogus
    ;; one could check for eof, but that would preclude pipelined interaction
    ;; (raw-post-data :force-binary t)
    (http:response-content-stream response)))
