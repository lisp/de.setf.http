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

(defmethod http:request-auth-token ((request request))
  (header-in :auth_token request))

(defmethod http:request-authentication ((request request))
  (authorization request))

(defmethod http:request-session-id ((request request))
  (cookie-in (session-cookie-name (request-acceptor request))
             request))

(defmethod http:query-field-value ((request request) key)
  (get-parameter key request))


(defmethod http:request-content-length ((request tbnl-request))
  (let ((header (content-length request)))
    (when (plusp (length header)) (parse-integer header))))

(defmethod http:request-content-stream ((request tbnl-request))
  (content-stream request))

(defmethod http:response-status-code ((response tbnl-response))
  (return-code response))

(defmethod (setf http:response-status-code) (code (response tbnl-response))
  (setf (return-code response) code))

(defmethod (setf http:response-content-type) (content-type (response tbnl-response))
  (setf (header-out :content-type response) content-type))

(defmethod (setf http:response-content-type) ((mime-type mime:mime-type) (response tbnl-response))
  (setf (http:response-content-type response) (mime:mime-type-expression mime-type)))

(defmethod (setf http:response-character-encoding) (character-encoding (response tbnl-response))
  (setf (header-out :character-encoding response) character-encoding))

(defmethod (setf http:response-location-header) (location (response tbnl-response))
  (setf (header-out :location response) location))

(defmethod (setf http:response-www-authenticate-header) (authentication-method (response tbnl-response))
  (setf (header-out :www-authenticate response) authentication-method))

(defmethod (setf http:response-retry-after-header) (time (response tbnl-response))
  (setf (header-out :retry-after response) time))

(defmethod http:request-path ((request tbnl-request))
  (script-name request))

(defmethod http:request-line-method ((request tbnl-request))
  (request-method request))

(defmethod http:request-acceptor ((request tbnl-request))
  (request-acceptor request))

(defmethod http:request-accept-header ((request tbnl-request))
  (header-in :accept request))

(defmethod http:request-content-type-header ((request tbnl-request))
  (header-in :content-type request))

(defclass tbnl-acceptor (http:acceptor acceptor)
  ())



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
                               (unless (http:response-headers-sent-p *reply*)
                                 (http:report-condition-headers c *reply*)
                                 (http:send-headers *reply*))
                               (return-from process-connection
                                 (values nil c nil))))
             (error (lambda (c)
                      (http:log *lisp-errors-log-level* acceptor "unhandled error in http response: ~a" c)
                      (format *error-output* "~%~a" (get-backtrace))
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
                  (let* ((http:*request* (http:make-request acceptor
                                                            :socket socket
                                                            :headers-in headers-in
                                                            :content-stream *hunchentoot-stream*
                                                            :method method
                                                            :uri url-string
                                                            :server-protocol protocol))
                         (*request* http:*request*)
                         (*reply* (http:make-response acceptor
                                                      :request *request*
                                                      :keep-alive-p (keep-alive-p *request*)
                                                      :server-protocol protocol
                                                      :content-stream *hunchentoot-stream*))
                         (http:*response* *reply*)
                         (*tmp-files* nil)
                         (*headers-sent* nil)
                         (*session* nil))
                    (with-acceptor-request-count-incremented (acceptor)
                      (catch 'request-processed
                        (http:respond-to-request acceptor http:*request* *reply*)))
                    (finish-output (http:response-content-stream *reply*))
                    ;; access log message
                    (acceptor-log-access acceptor :return-code (http:response-status-code *reply*)))
                  (finish-output *hunchentoot-stream*)
                  
                  (setq *hunchentoot-stream* (reset-connection-stream *acceptor* *hunchentoot-stream*))
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


;;;
;;; the native hunchentoot control structure is
;;; send-headers
;;; -> send-output
;;;    -> send-response
;;; in which the resonse status line and the headers are actually emitted by
;;; send-response, with argument presence indicating the intended control-flow.
;;; all, somewhat less than intuitive: it reads as if the invocation through
;;; send-output ues the output stream and sens content or just prepares the
;;; stream, based on content presence while send-headers indicates with null
;;; content, the intent to emit headers only.
;;;
;;; rather than depend on argument value side-effects the two functions
;;;   send-headers
;;;   send-entity-body
;;; do just that. the application is to invoke them in the correct order, or
;;; leave it to the first reference to the response content stream to trigger
;;; header generation



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
  
  (unless (http:response-headers-sent-p response)
    (let ((header-stream (flex:make-flexi-stream (http:response-content-stream response)
                                                 :external-format +latin-1+))
          (content-length nil)
          (external-format +utf-8+)
          (head-request-p (eq :head (request-method (http:response-request response))))
          (server nil)
          (date nil)
          (status-code (http:response-status-code response)))
      
      ;; emit the response and entity headers
      ;; start with status line
      (format header-stream "HTTP/1.1 ~D ~A~C~C" status-code (reason-phrase status-code) #\Return #\Linefeed)
      ;; write all headers from the REPLY object
      
      (loop for (key . value) in (headers-out response)
            when value
            do (case key
                 (:content-type (when (mime:binary-mime-type-p value)
                                  (setf external-format nil)))
                 (:content-length (setf content-length value))
                 (:date (setf date t))
                 (:server (setf server t)))
            and do (write-header-line (as-capitalized-string key) value header-stream))
      (unless date
        (write-header-line (as-capitalized-string :date) (rfc-1123-date) header-stream))
      (unless server
        (write-header-line (as-capitalized-string :server) (acceptor-server-name (http:response-acceptor response)) header-stream))
      (let ((chunked-p (and (acceptor-output-chunking-p (http:response-acceptor response))
                            (eq (http:response-protocol response) :http/1.1)
                            ;; only turn chunking on if the content
                            ;; length is unknown at this point...
                            (null content-length))))
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
          (when chunked-p
            (write-header-line (as-capitalized-string :transfer-encoding) "chunked" header-stream))
          ;; now emit keep-alive headers
          (cond (keep-alive-p
                 (setf *close-hunchentoot-stream* nil)
                 (when (and (acceptor-read-timeout (http:response-acceptor response))
                            (or (not (eq (server-protocol response) :http/1.1))
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
      
        ;; depending on whether content length was set and/or the content-type
        ;; adjust and cache the entity body stream
        (let* ((body-stream (cond (external-format
                                   (unless (eq external-format +latin-1+)
                                     (setf (flex:flexi-stream-external-format header-stream) external-format))
                                   header-stream)
                                  (t
                                   (http:response-content-stream response)))))
          (when chunked-p
            (unless (typep body-stream 'chunked-stream)
              (setq body-stream (make-chunked-stream body-stream)))
            (setf (chunked-stream-output-chunking-p body-stream) t))
          (setf (http:response-content-stream response) body-stream)
          
          ;; Read post data to clear stream - Force binary mode to avoid OCTETS-TO-STRING overhead.
          (raw-post-data :force-binary t)
          body-stream)))))



(defmethod http:send-entity-body ((response tbnl-response) (content sequence))
  (unless (http:response-headers-sent-p response)
    (setf (content-type*) (maybe-add-charset-to-content-type-header (content-type*) (reply-external-format*)))
    (setf (content-length*) (length content)))
  (write-sequence content (http:response-content-stream response)))
