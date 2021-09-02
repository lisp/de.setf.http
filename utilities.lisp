;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.http.implementation; -*-
;;; Copyright 2013 [james anderson](mailto:james.anderson@setf.de) All Rights Reserved

(in-package :de.setf.http.implementation)

;;; read-only state

(declaim (ftype (function () http:acceptor) http:acceptor))
(defun http:request () http:*request*)
(define-compiler-macro http:request () 'http:*request*)
(defun http:resource () http:*resource*)
(define-compiler-macro http:resource () 'http:*resource*)
(defun http:response () http:*response*)
(define-compiler-macro http:response () 'http:*response*)

(defun http-verb-p (object)
  (and (keywordp object)
       (eq (find-symbol (symbol-name object) *http-method-package*) object)))

(defun http-verb-list-p (object)
  (and (consp object)
       (every #'http-verb-p object)))

(cffi:defcfun ("openlog" %openlog) :void (id :string) (options :int) (facility :int))
(cffi:defcfun ("syslog" %syslog) :void (priority :int) (c-format :string) (c-message :string))

(defun log-level-qualifies? (level)
  (find level (member http:*log-level* http:*log-levels*)))

(defun call-if-log-level-qualifies (level operator)
  "Given a log LEVEL and an OPERATOR, call the operator iff the log level is satisfied."
  (declare (dynamic-extent operator))
  (when (log-level-qualifies? level)
    (funcall operator)))

(macrolet ((def-log-op (level)
             `(progn
                (defmacro ,(intern (concatenate 'string (string :log-) (string level)) :http) (format-control &rest args)
                  `(flet ((call-log ()
                            (http:log ,,level *log-destination* ,format-control ,@args)
                            t))
                     (declare (dynamic-extent (function call-log)))
                     (call-if-log-level-qualifies ,,level (function call-log)))))))
  (def-log-op :fatal)
  (def-log-op :critical)
  (def-log-op :error)
  (def-log-op :warn)
  (def-log-op :notice)
  (def-log-op :info)
  (def-log-op :debug)
  (def-log-op :trace))

(defun syslog-level (level)
    (or (rest (assoc level (load-time-value `((:trace . 7)
                                              (:debug . 7)
                                              (:info . 6)
                                              (:notice . 5)
                                              (:warn . 4)
                                              (:error . 3)
                                              (:critical . 2)
                                              (:fatal . 1)))))
        3))

(defun write-syslog (level format-control &rest args)
  (handler-case (let* ((*print-pretty* nil)
                       (*print-length* 10)
                       (message (apply #'format nil format-control args)))
                  (#+sbcl sb-sys:without-interrupts #-sbcl progn
                   (cffi:with-foreign-strings ((%cformat "%s")
                                               (%message message))
                     (%syslog (syslog-level level) %cformat %message)))
                  ;; write anything above :notice to the terminal, if present
                  (when (and (not (find :notice (member level http:*log-levels*)))
                             *trace-output*
                             (interactive-stream-p *trace-output*))
                    (format *trace-output* "~&;;;~a~%" message)))
    (error (error)
           (setq *log-condition* error)
           nil)))

(defgeneric http:log (level destination format-control &rest arguments)
  (:documentation
    "Emit a log message to the destination.")
  (:method (level (destination null) format-control &rest arguments)
    (apply #'http:log level *trace-output* format-control arguments))

  (:method (level (destination stream) format-control &rest arguments)
    (declare (dynamic-extent arguments))
    (let ((*print-pretty* nil)
          (*print-length* 10))
      (format destination "~&;;; [~a] ~?" level format-control arguments)))

  (:method (level (destination (eql :syslog)) format-control &rest arguments)
    (apply #'write-syslog level format-control arguments))
  )

;;; gray stream compatibility

#+sbcl
(defmethod stream-file-position ((stream t))
  ;; for anything which has no implementation, delegate it to the sbcl implementation
  (sb-gray:stream-file-position stream))

;;;
;;; media type support
;;; implement media type unions and resolution

;;; unions

(let ((mime-token '(:greedy-repetition 1 nil (:inverted-char-class #\, #\; #\=)))
      (mime-major-minor '(:sequence
                          (:register (:greedy-repetition 1 nil (:inverted-char-class #\/)))
                          #\/
                          (:register (:greedy-repetition 1 nil (:inverted-char-class #\, #\;)))))
      (mime-q '(:sequence (:char-class #\q #\q) #\= (:register (:greedy-repetition 1 nil (:char-class #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\.)))))
      (mime-accept-params '(:sequence #\; (:alternation mime-q mime-token)))
      (mime-range-and-parameters '(:sequence mime-major-minor (:greedy-repetition 0 nil mime-accept-params))))
                                   
  (setf (cl-ppcre:parse-tree-synonym 'mime-major-minor) mime-major-minor)
  (setf (cl-ppcre:parse-tree-synonym 'mime-token) mime-token)
  (setf (cl-ppcre:parse-tree-synonym 'mime-q) mime-q)
  (setf (cl-ppcre:parse-tree-synonym 'mime-accept-params) mime-accept-params)
  (setf (cl-ppcre:parse-tree-synonym 'mime-range-and-parameters) mime-range-and-parameters))


(defparameter *content-coding-pattern* (cl-ppcre:create-scanner "([^,;=]+)(?:;q=([^,]*))?"))
(defparameter *accept-range-pattern* (cl-ppcre:create-scanner 'mime-range-and-parameters))

;;; (cl-ppcre:scan-to-strings *accept-range-pattern* "text/html")
;;; (cl-ppcre:scan-to-strings *accept-range-pattern* "text/html;q=1;a=b")
;;; (cl-ppcre:scan-to-strings *accept-range-pattern* "text/html,application/json,application/rdf+xml")
;;; (cl-ppcre:scan-to-strings *accept-range-pattern* "text/html;q=0.2,application/json,application/rdf+xml")
;;; (cl-ppcre:scan-to-strings *content-coding-pattern* "gzip;q=0.2,identity")

(defun parse-media-range (range)
  (coerce (nth-value 1 (cl-ppcre:scan-to-strings *accept-range-pattern* range)) 'list))

(defun parse-content-coding (coding)
  (coerce (nth-value 1 (cl-ppcre:scan-to-strings *content-coding-pattern* coding)) 'list))

(defun qvalue-char-p (c)
  (or (digit-char-p c) (eql c #\.)))

(defun parse-qvalue (qvalue)
  (if (every #'qvalue-char-p qvalue)
    (read-from-string qvalue)
    (http:bad-request "Invalid qvalue: '~a'" qvalue)))

#+(or) ; delegate most of the work to the mime type implementation
(defun compute-accept-ordered-types (header)
  (let* ((accept-ranges (split-string header #\,))
         (types (loop for range in accept-ranges
                        for (major minor q) = (or (parse-media-range range)
                                                  (http:bad-request "Invalid accept range: ~s." range))
                        for type = (dsu:intern-mime-type-key (format nil "~a/~a" major minor) :if-does-not-exist nil)
                        for quality = (cond (q (parse-qvalue q))
                                            (t 1))
                        if type
                        collect (cons type quality)
                        else
                        do (http:log-warn "The mime type '~a/~a' is not defined." major minor))))
    (if types
      (mapcar #'first (sort types #'> :key #'rest))
      (http:not-acceptable "Unacceptable accept ranges: '~a'" header))))

(defun compute-accept-ordered-types (header)
  (handler-case
      (let* ((accept-ranges (split-string header #\,))
             (types (loop for range in accept-ranges
                      for type = (mime:mime-type range :if-does-not-exist nil)
                      if type
                      collect type)))
        ;;else do (http:log-warn "The mime type '~a' is not defined." range))))
        (if types
            (sort types #'> :key #'mime::mime-type-quality)
            (http:not-acceptable "Unacceptable accept ranges: '~a'" header)))
    (error (c)
           (http:bad-request "Invalid accept header: ~a" c))))

(defun compute-accept-encoding-ordered-codings (header)
  ;; translate the string into a sorted, qualified a-list
  (stable-sort (loop for element in (split-string (remove #\space header) ",")
                     for (content-coding qvalue) = (parse-content-coding element)
                     if content-coding
                     collect (cons content-coding (if qvalue (parse-qvalue qvalue) 1))
                     else do (http:bad-request "Invalid content coding: ~s." element))
               #'> :key #'rest))
  


;;; (compute-accept-ordered-types "text/html")
;;; (compute-accept-ordered-types "*/*")
;;; (http.i::compute-accept-ordered-types "application/trig;q=1.0,application/n-quads;q=0.7,text/turtle;q=0.6,application/n-triples;q=0.3,text/n3;q=0.2")
;;; (parse-content-coding "compress")
;;; (compute-accept-encoding-ordered-codings "compress, gzip")
;;; (compute-accept-encoding-ordered-codings nil)
;;; (compute-accept-encoding-ordered-codings "")
;;; (compute-accept-encoding-ordered-codings "*")
;;; (compute-accept-encoding-ordered-codings "compress;q=0.5, gzip;q=1.0")
;;; (compute-accept-encoding-ordered-codings "gzip;q=1.0, identity; q=0.5, *;q=0")


(defun concrete-media-type (mime-type)
  "return an instance of the initial class in the union precedence list"
  (symbol-value (class-name (first (c2mop:class-direct-superclasses (class-of mime-type))))))

;;; codecs

#+(or)
(defmethod stream-listen ((stream SB-SYS:FD-STREAM)) (not (sb-impl::sysread-may-block-p stream)))

(defgeneric http:copy-stream (input-source output-destination. &key length)
  (:documentation "Copy the content of the input-source to the output-destination.
   The bsae combination is (stream x stream), in which case a byt copy is performed and
   the source is checked for excess input.
   Combinations, such as (stream x pathname) and (pathname x stream), delegate to the
   base combination.
   Combinations with a sequence destination are implemented respective the destination element type.
   nb. an implementation base on utiltity functions, such as alexandria:copy-stream, which presume the element type
   in order to buffer results, will fail on multivalent streams which change type.
   nb. using listen at the outset in order to avoid hanging on missing content, is actually a race situation,
   as the content can be delayed, as is the case for interative source streams.")

  (:method ((input-stream stream) (output-stream stream) &key length)
    (unless length
      (setf length most-positive-fixnum))

    (cond ((plusp length)
           ;; do not listen.
           ;; that does a read-char-no-hang which leaves a state which read-byte does not handle (listen input-stream))
           ;; on a completed ssl stream, it will have been closed, which signals an error
           (let* ((count 0))
             (declare (type fixnum count length))
             (multiple-value-bind (reader reader-arg) (stream-binary-reader input-stream)
               (multiple-value-bind (writer writer-arg) (stream-binary-writer output-stream)
                 (loop for byte = (funcall reader reader-arg)
                   do (cond (byte
                             (when (> (incf count) length)
                               (http:request-entity-too-large "Limit of ~d bytes exceeded." length))
                             (funcall writer writer-arg byte))
                            (t
                             (return))))))
             count))
          (t
           0)))
  
  (:method ((input-stream stream) (output pathname) &rest args)
    (declare (dynamic-extent args))
    (handler-case (with-open-file (output-stream output :direction :output :if-exists :supersede :if-does-not-exist :create
                                                 :element-type 'unsigned-byte)
                    (apply #'http:copy-stream input-stream output-stream args))
      (error (c)
        ;; if the copy fails, ensure that the file is removed
        (when (probe-file output) (delete-file output))
        (error c))))

  (:method ((input pathname) (output-stream stream) &rest args)
    (declare (dynamic-extent args))
    (handler-case (with-open-file (input-stream input :direction :input :element-type 'unsigned-byte)
                    (apply #'http:copy-stream input-stream output-stream args))))

  ;; read into a binary buffer
  (:method ((input-stream stream) (content vector) &key length)
    (unless (and length (= length (length content)))
      (assert (adjustable-array-p content) ()
              "Destination sequence must either of the specified length or be adjustable for chunked content: ~a."
              (type-of content))
      (setf length most-positive-fixnum))
    (cond ((plusp length)
           (let* ((count 0))
             (declare (type fixnum count length))
             (multiple-value-bind (reader reader-arg) (stream-binary-reader input-stream)
               (loop for char = (funcall reader reader-arg)
                 while char
                 when (>= count (length content))
                 do (setf content (adjust-array content (list (+ count 1024))))
                 do (setf (aref content count) char)
                 until (>= (incf count) length)))
             (when (listen input-stream)
               (http:request-entity-too-large "Limit of ~d bytes exceeded." length))
             (when (and (adjustable-array-p content)
                        (< count (length content)))
               (adjust-array content count))
             count))
          (t
           (when (adjustable-array-p content)
             (setf content (adjust-array content 0)))
           0)))

  ;; read into a string (character buffer)
  (:method ((input-stream stream) (content string) &key length)
    (unless (and length (= length (length content)))
      (assert (adjustable-array-p content) ()
              "Destination sequence must either of the specified length or be adjustable for chunked content: ~a."
              (type-of content))
      (setf length most-positive-fixnum))
    (cond ((plusp length)
           (let* ((byte-count 0)
                  (character-count 0))
             (declare (type fixnum byte-count character-count length))
             (multiple-value-bind (reader reader-arg) (stream-reader input-stream)
               (loop (multiple-value-bind (char size)
                                          (funcall reader reader-arg)
                       (declare (type fixnum size))  ;; @ eof should be 0
                       (unless char (return))
                       (when (>= character-count (length content))
                         (setf content (adjust-array content (list (+ (length content) 1024)))))
                       (setf (char content character-count) char)
                       (incf character-count)
                       (when (>= (incf byte-count size) length)
                         (return)))))
             (when (listen input-stream)
               (http:request-entity-too-large "Limit of ~d bytes exceeded." length))
             (when (and (adjustable-array-p content)
                        (< character-count (length content)))
               (setf content (adjust-array content (list character-count))))
             (values byte-count content)))
          (t
           (when (adjustable-array-p content)
             (setf content (adjust-array content 0)))
           0)))

  ;; for convenience use. in a request, it is wrapped and copied with a decoder, above
  (:method ((input-stream de.setf.utility.implementation::vector-input-stream) (content string) &key length)
    (unless (and length (= length (length content)))
      (assert (adjustable-array-p content) ()
              "Destination sequence must either of the specified length or be adjustable for chunked content: ~a."
              (type-of content))
      (setf length most-positive-fixnum))
    (cond ((plusp length)
           (loop for byte = (stream-read-byte input-stream)
             while (integerp byte)
             do (vector-push-extend (code-char byte) content))
           (values (length content) content))
          (t
           (when (adjustable-array-p content)
             (setf content (adjust-array content 0)))
           0)))

  #+(or)                                ; unused method which tries to do the unchunking in-line
  (:method ((input-stream stream) (content vector) &key length)
    (assert (equalp (array-element-type content) (stream-element-type input-stream)) ()
            "Destination sequence type does not agree with the source stream element type: ~s: ~s."
            (array-element-type content) (stream-element-type input-stream))
    ;; if the stream is chunking, chunk in place within the limit.
    ;; if it is not, read exactly that number of
    (cond ((chunga:chunked-stream-input-chunking-p input-stream)
           ;; read chunked up to the limit, allowing that the chunking buffers intermediate, which
           ;; since this must allow for utf/character decoding, is not to be avoided. alternative to
           ;; get-post-data, however, do not interpose yet another copy
           (assert (adjustable-array-p content) ()
                   "Destinaton sequence must be adjustable for chunked content: ~a."
                   (type-of content))
           (loop for start = 0 then end
                 ;; determine the next chunk size; when none further, return the accumulation
                 for chunk-size = (cond ((stream-listen input-stream)
                                         (chunga::chunked-stream-input-limit input-stream))
                                        (t
                                         (adjust-array content '(0))
                                         (return 0)))
                 then (cond ((stream-listen input-stream)
                             (chunga::chunked-stream-input-limit input-stream))
                            (t
                             (return end)))
                 ;; and, from that, the required additional space
                 for end = (+ start chunk-size) then (+ end chunk-size)
                 ;; when there remains to be read, apply the length constraint
                 do (progn (when (and length (> end length))
                             (http:request-entity-too-large "Limit of ~d ~:[characters~;bytes~] exceeded."
                                                            length
                                                            (eq (stream-element-type input-stream) 'character)))
                           ;; ensure space for additional content
                           (unless (>= (length content) end)
                             (adjust-array content (list end)))
                           ;; read the content in to the empty sub-sequence
                           (unless (= end (setf end (stream-read-sequence input-stream content start end)))
                             ;; in preparation to return, in the event, that, either utf8 decoding yielded
                             ;; fewer characters than octets, or the original sequence length exceeded the
                             ;; actual content length
                             (adjust-array content end)))))
          (t
           ;; read an unchunked stream of the given length, or to the length of the sequence
           (when (and length (not (= length (length content))))
             (assert (adjustable-array-p content) ()
                     "Destinaton sequence length does not agree with length constraint: ~d: ~a."
                     length (type-of content))
             (adjust-array content (list length)))
           (let ((end (read-sequence content input-stream :start 0 :end (length content))))
             (unless (= end (length content))
               ;; premature eof
               (adjust-array content (list  end)))
             end)))))





(defgeneric http:decode-request (resource request content-type)
  (:documentation "Implements the default behavior for resource function
    encoding methods when they are declared without a body.
    The default method signals an unsupported content type error.")
  #+(or)
  (:method :around ((resource t) (request t) (content-type t))
    (map nil 'print (compute-applicable-methods #'http:decode-request (list resource request content-type)))
    (call-next-method))
  (:method ((resource t) (request t) (content-type t))
    "The default method signals an unsupported content type error"
    (http:unsupported-media-type :text (format nil "resource (~a) type/method combination not supported: ~a/~a"
                                               (_slot-value resource 'path)
                                               (_slot-value request 'method)
                                               (type-of content-type)))))
                                               

;;; times

(defparameter *rdf1123-date-time-scanner*
  (cl-ppcre:create-scanner "([a-zA-z]{3}), ([0-9]{2}) ([a-zA-z]{3}) ([0-9]{4}) ([0-9]{2}):([0-9]{2}):([0-9]{2}) GMT"))
;;; (nth-value 1 (cl-ppcre:scan-to-strings *rdf1123-date-time-scanner* "Wed, 30 May 2007 18:47:52 GMT"))

(defun http:parse-rfc1123 (value &key junk-allowed)
  (let ((substrings (coerce (nth-value 1 (cl-ppcre:scan-to-strings *rdf1123-date-time-scanner* value)) 'list)))
    (if substrings
      (destructuring-bind (wkday day month year hour minute second)
                          substrings
        (declare (ignore wkday))
        (encode-universal-time (parse-integer second)
                               (parse-integer minute)
                               (parse-integer hour)
                               (parse-integer day)
                               (date:decode-month-name month)
                               (parse-integer year)
                               0))
      (unless junk-allowed
        (error "Invalid rdf1123 date-time: ~s." value)))))

(defgeneric http:encode-rfc1123 (value &optional stream)
  (:method ((value integer) &optional (stream nil))
    (multiple-value-bind (second minute hour day month year weekday)
                         (decode-universal-time value 0)
      (format stream "~a, ~d ~:(~a~) ~4,'0d ~2,'0d:~2,'0d:~2,'0d GMT"
              (date:day-in-week-name weekday 3) day (date:month-name month 3) year hour minute second))))


#|
(let ((stream (make-string-input-stream (coerce (loop for i from 0 below 26 collect (code-char (+ (char-code #\a) i))) 'string)))
      (content (make-array 26 :element-type 'character :adjustable t :initial-element #\.)))
  (list (http:copy-stream stream content) content))

(let ((stream (make-string-input-stream (coerce (loop for i from 0 below 26 collect (code-char (+ (char-code #\a) i))) 'string)))
      (content (make-array 26 :element-type 'character :adjustable t  :initial-element #\.)))
  (list (http:copy-stream stream content :length 10) content))

(let ((stream (make-string-input-stream (coerce (loop for i from 0 below 10 collect (code-char (+ (char-code #\a) i))) 'string)))
      (content (make-array 26 :element-type 'character :adjustable t  :initial-element #\.)))
  (list (http:copy-stream stream content :length 26) content))

(let* ((input (flexi-streams:make-in-memory-input-stream (map 'vector #'char-code
                                                              (concatenate 'list
                                                                           '(#\4 #\return #\newline) "Wiki" '(#\return #\newline)
                                                                           '(#\5 #\return #\newline) "pedia" '(#\return #\newline)
                                                                           '(#\e #\return #\newline) " in" '(#\return #\newline #\return #\newline) "chunks." '(#\return #\newline)
                                                                           '(#\0 #\return #\newline) '(#\return #\newline)))))
       (stream (chunga:make-chunked-stream input))
       (content (make-array 26 :element-type '(unsigned-byte 8) :adjustable t  :initial-element (char-code #\.))))
  (setf (chunga:chunked-stream-input-chunking-p stream) t)
  (list (http:copy-stream stream content :length 26) (map 'string #'code-char content)))

(let* ((input (flexi-streams:make-in-memory-input-stream (map 'vector #'char-code
                                                              (concatenate 'list
                                                                           '(#\4 #\return #\newline) "Wiki" '(#\return #\newline)
                                                                           '(#\5 #\return #\newline) "pedia" '(#\return #\newline)
                                                                           '(#\e #\return #\newline) " in" '(#\return #\newline #\return #\newline) "chunks." '(#\return #\newline)
                                                                           '(#\0 #\return #\newline) '(#\return #\newline)))))
       (stream (chunga:make-chunked-stream input))
       (content (make-array 10 :element-type '(unsigned-byte 8) :adjustable t  :initial-element (char-code #\.))))
  (setf (chunga:chunked-stream-input-chunking-p stream) t)
  (list (http:copy-stream stream content :length 26) (map 'string #'code-char content)))



alternative for mime types, to resolve immediately to a concrete type
;;;
;;; resolve accept mime type

(defparameter *accept-specification-scanner*
  (cl-ppcre:create-scanner '(:sequence
                             (:register (:sequence (:greedy-repetition 1 nil (:char-class (:range #\A #\Z) (:range #\a #\z) #\*))
                                                   #\/
                                                   (:greedy-repetition 1 nil (:char-class :word-char-class #\+ #\- #\*))))
                             (:GREEDY-REPETITION 0 1 (:sequence #\; #\q #\= (:register (:sequence (:greedy-repetition 1 nil (:char-class (:range #\0 #\9)))
                                                                                                  #\.
                                                                                                  (:greedy-repetition 1 nil (:char-class (:range #\0 #\9)))))))
                             (:GREEDY-REPETITION 0 1 #\,))))
(defun parse-accept-specification  (accept-string)
  (let ((accept-list ()))
    (cl-ppcre:do-scans (binding-start bindings-end starts ends *accept-specification-scanner* accept-string)
                       (push (cons (subseq accept-string (aref starts 0) (aref ends 0))
                                   (if (and (aref starts 1) (> (aref ends 1) (aref starts 1)))
                                     (meta:parse-float (subseq accept-string (aref starts 1) (aref ends 1)))
                                     1.0))
                             accept-list))
    (sort accept-list #'> :key #'rest)))
;;; (cl-ppcre:scan-to-strings *accept-specification-scanner* "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8,application/rdf+xml;q=0.93,text/rdf+n3;q=0.5")
;;; (parse-accept-specification "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8,application/rdf+xml;q=0.93,text/rdf+n3;q=0.5")

(defgeneric select-mime-type (accept-specification mime-types)
  (:method ((accept-specification null) (mime-types list))
    (first mime-types))
  (:method ((accept-string string) mime-types)
    (select-mime-type (mapcar #'first (parse-accept-specification accept-string)) mime-types))
  (:method ((accept-list list) (mime-types list))
    (loop for accept-mime-type-string in accept-list
          for accept-mime-type = (cond ((ignore-errors (mime-type accept-mime-type-string)))
                                       (t
                                        (log-warn "Unsupported mime type: ~s." accept-mime-type-string)
                                        nil))
          when (find accept-mime-type mime-types)
          do (return accept-mime-type)
          finally (return (first mime-types)))))
;;; (select-mime-type "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8,application/rdf+xml;q=0.93,text/rdf+n3;q=0.5" (list mime:text/plain mime:application/link-format mime:*/*))
;;; (select-mime-type "" (list mime:text/plain mime:application/link-format mime:*/*))


|#