;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.http.implementation; -*-
;;; Copyright 2013 [james anderson](mailto:james.anderson@setf.de) All Rights Reserved

(in-package :de.setf.http.implementation)

(:documentation "chunked streams with codecs"
 "An HTTP stream has limited, well-defined tasks, for which layering and optional extensions
 add little to the performance. Instead of using the structure

   (write (encode object external-format) (chunking (stream socket)))

 structure - with the analogous read mechanism, this implementation adopts the simpler form

   (write object (chunking+encoding (stream socket)))

 whereby, there is no reason to not ultimately integrate the chunking+encoding mechanism directly
 in the socket stream, should the overhead be prohibitive, but that is likely worthwhile only
 if one bothers to work directly with the network buffers.

 much of the implementation is adapted from the de.setf.amqp streams.

 header output is delayed until the first content byte is written.
 at that point an output stream encodes those headers which have been recorded in the request instance and
 buffers them in a string stream.
 this allows for any initial headers to be replaced with those which indicate a condition which is signaled
 while content is being prepared, but before the body output has commenced.
 it also makes the value directly available for broadcast streams when transcribing.
 up to this point, the buffer stream can be reset and alternative headers introduced.
 after that point - that is, once body content has been written that is no longer possible
 and the response will not reflect the error.

 the dependency is such that, at the point when the http:output-stream instance is written, it delegates to
 an expected dynamic binding for http:*request*, which in turn encodes its headers to the stream then bound
 as its content stream - which may not be the triggering http:output-stream, but instead something which
 wraps it.
 in addition, stream-finish-header-output is invoked by writing the final chunk the response stream to
 ensure that they are written when there is no content written
")

(defclass http:stream (stream)
  ((media-type
    :initarg :media-type
    :reader http:stream-media-type :writer setf-stream-media-type
    :documentation "Binds the media type instance, whichh encapsulates the
     character encoding. The setf operator modifies the codec operators as
     a sode-effect.")
   (encoder
    :type function
    :reader  stream-encoder :writer setf-stream-encoder
    :documentation "Binds a function which is then used to encode character
     values for output to the stream. If no media type is specified, text/plain
     with utf-8 encoding is used. if the media type is binary, the encoide for iso-8859-1
     is used.")
   (decoder
    :type function
    :reader stream-decoder :writer setf-stream-decoder
    :documentation "Optionally binds a function which is then used to decode
     character values from input from the stream.")
   (eol-marker
    :initform #\newline :initarg :eol-marker
    :accessor stream-eol-marker))
  (:default-initargs
    :media-type (make-instance 'mime:text/plain :charset :utf-8)))

(defclass http:input-stream (http:stream chunga:chunked-input-stream)
  ((unread-characters
    :initform ()
    :accessor stream-unread-characters)
   (line-buffer
    :initform (make-array 32 :element-type 'character :fill-pointer 0 :adjustable t)
    :reader stream-line-buffer)))

(defclass http:output-stream (http:stream chunga:chunked-output-stream)
  ((header-stream
    :initform (make-string-output-stream)
    :accessor http:stream-header-stream
    :documentation "A string stream to buffer headers until the content is actually sent
     in order that (some) errors can suppress the initial headers and instead emit an
     error code. Once the headers are emitted, the slot is cleared to indicate that
     no change is possible.")
   (byte-writer
    :type function
    :reader stream-byte-writer
    :documentation "Binds a function which is then used to write a byte to the stream.
    When set as chunked, appends to and flushed the chunk buffer. Otherwise write through to
    the reference stream.
    The initial value flushes headers to the reference stream and computes the effective
    value given the chunking setting.")
   (byte-writer-arg
    :type t
    :reader stream-byte-writer-arg
    :documentation "The auxiliary argument for the binary writer function respective the
    current chunking setting: for chunked the stream itself, for non/chunked the reference
    stream.")
   (char-writer
    :type function
    :reader stream-char-reader
    :documentation "")
   (char-writer-arg
    :type t
    :reader stream-char-reader-arg
    :documentation "")))


(defmethod initialize-instance :after ((instance http:stream) &key)
  (update-stream-codecs instance))

(defmethod initialize-instance :after ((instance http:output-stream) &key)
  (initialize-stream-writers instance))

(defmethod stream-direction ((stream http:output-stream))
  :output)

(defmethod stream-direction ((stream http:input-stream))
  :input)

(defmethod stream-element-type ((stream http:stream))
  "Return the element type corresponding to the current media type"
  (if (mime:binary-mime-type-p (http:stream-media-type stream))
    '(unsigned-byte 8)
    'character))


(defgeneric (setf http:stream-media-type) (type stream)
  (:method ((type mime:mime-type) (stream http:stream))
    (let ((old-type (http:stream-media-type stream)))
      (setf-stream-media-type type stream)
      (unless (and old-type
                   (equalp (mime:mime-type-charset old-type) (mime:mime-type-charset type)))
        (slot-makunbound stream 'decoder)
        (slot-makunbound stream 'encoder)
        (unless (mime:binary-mime-type-p type)
          (update-stream-codecs stream))))
    type)
  (:method ((type t) (stream http:stream))
    (setf (http:stream-media-type stream) (mime:mime-type type))))

 
(defgeneric update-stream-codecs (stream)
  (:method ((stream http:stream))
    (let ((media-type (http:stream-media-type stream)))
      (multiple-value-bind (decoder encoder sizer)
                           (compute-charset-codecs media-type)
        (flet ((sized-decoder (get-byte source)
                 (let ((char (funcall decoder get-byte source)))
                   (if char
                       (values char (funcall sizer char))
                       (values nil 0))))
               (constant-size-decoder (get-byte source)
                 (let ((char (funcall decoder get-byte source)))
                   (if char
                       (values char sizer)
                       (values nil 0))))
               (sized-encoder (char put-byte destination)
                 (values (funcall encoder char put-byte destination)
                         (funcall sizer char)))
               (constant-size-encoder (char put-byte destination)
                 (values (funcall encoder char put-byte destination)
                         sizer)))
          (unless sizer (setf sizer 1))
          (unless (slot-boundp stream 'decoder)
            (setf-stream-decoder (etypecase sizer
                                   (integer #'constant-size-decoder)
                                   ((or symbol function) #'sized-decoder))
                                 stream))
          (unless (slot-boundp stream 'encoder)
            (setf-stream-encoder (etypecase sizer
                                   (integer #'constant-size-encoder)
                                   ((or symbol function) #'sized-encoder))
                                 stream))))
      media-type)))


(defmethod (setf chunga:chunked-stream-output-chunking-p) :after ((new-value t) (stream http:output-stream))
  "update the writer configuration to reflect the changed chunking setting"
  (update-stream-writers stream))


(defgeneric initialize-stream-writers (stream)
  (:documentation
   "Set up initial writer functions which first ensure that the headers have been generated and
flushed to the base output stream, then install the writers respective the content type, then
invoke the respective content writer on the arguments from the triggering call.")
  (:method ((stream http:output-stream))
    (with-slots (byte-writer byte-writer-arg char-writer char-writer-arg) stream
      (flet ((initial-byte-writer (byte-stream byte)
               (unless (eq byte-stream stream)
                 (warn "initialize-stream-writers: initial byte stream mismatch: ~s != ~s:"
                       byte-stream stream))
               (stream-finish-header-output stream)
               (update-stream-writers stream)
               (funcall byte-writer byte-writer-arg byte))
             (initial-char-writer (char-stream char)
               (unless (eq char-stream stream)
                 (warn "initialize-stream-writers: initial char stream mismatch: ~s != ~s:"
                       char-stream stream))
               (stream-finish-header-output stream)
               (update-stream-writers stream)
               (funcall char-writer char-writer-arg char)))
        (setf byte-writer #'initial-byte-writer
              byte-writer-arg stream)
        (setf char-writer #'initial-char-writer
              char-writer-arg stream)))))

(defgeneric update-stream-writers (stream) 
  (:documentation "Set the stream operators for binary writing to reflect the chunking setting.

   The binary writer reflects whether the stream is set for chunked transfer encoding in that it either
   appends data to the chunking buffer and triggers a buffer write once full, or bypasses buffering and
   writes through to the reference stream.")
  
  (:method ((stream http:output-stream))
    "replace the current writer functions with operators which chunk-or-not for binary output
 and encode through the binary operators for character output."
    (with-slots (byte-writer byte-writer-arg char-writer char-writer-arg) stream
      (if (chunked-stream-output-chunking-p stream)
        (flet ((always-chunked-stream-write-byte (stream byte)
                 "Write the byte presuming the sream is chunked.
                  The buffer is flushed if necessary." 
                 (with-slots (chunga::output-index chunga::output-buffer) stream
                   (when (>= chunga::output-index chunga::+output-buffer-size+)
                     (chunga::flush-buffer stream))
                   (setf (aref chunga::output-buffer chunga::output-index) byte)
                   (incf chunga::output-index)
                   byte)))
          (setf byte-writer #'always-chunked-stream-write-byte
                byte-writer-arg stream))
        (flet ((reference-stream-write-byte (reference-stream byte)
                 (write-byte byte reference-stream)))
          (setf byte-writer #'reference-stream-write-byte
                byte-writer-arg (chunked-stream-stream stream))))
      ;; reset the character writer in order to capture the new binary operator
      (let ((byte-writer byte-writer)
            (byte-writer-arg byte-writer-arg)
            (encoder (stream-encoder stream)))
        (flet ((char-write-byte (stream char)
                 (declare (ignore stream))
                 (funcall encoder char byte-writer byte-writer-arg)))
          (setf char-writer #'char-write-byte
                char-writer-arg stream))))))

;;;
;;; general manipulation


;;; close :inherited


;;; open-stream-p : inherited


(defmethod stream-advance-to-column ((stream http:stream) (column t))
  nil)

(defmethod stream-line-column ((stream http:stream))
  "Constantly nil."
  nil)


;;; stream-listen is that of chunked-stream


(defmethod stream-start-line-p ((stream http:stream))
  "Constantly nil."
  nil)


;;;
;;; input

(defun chunked-stream-read-byte (stream)
  ;; transliterated from stream-read-byte (chunked-input-stream)
  "Reads one byte from STREAM.  Checks the chunk buffer first, if
   input chunking is enabled.  Re-fills buffer is necessary."
  (cond ((chunked-stream-input-chunking-p stream)
         (when (or (chunga::chunked-input-available-p stream)
                   (chunga::fill-buffer stream))
           (with-slots (chunga::input-buffer chunga::input-index) stream
             (prog1 (aref chunga::input-buffer chunga::input-index)
               (incf chunga::input-index)))))
        ((read-byte (chunked-stream-stream stream) nil nil))
        (t
         nil)))

(defun always-chunked-stream-read-byte (stream)
  "Reads one byte from STREAM. Always checks the chunk buffer first.
   Re-fills buffer is necessary."
  (when (or (chunga::chunked-input-available-p stream)
            (chunga::fill-buffer stream))
    (with-slots (chunga::input-buffer chunga::input-index) stream
      (prog1 (aref chunga::input-buffer chunga::input-index)
        (incf chunga::input-index)))))


(defmethod stream-reader ((stream http:input-stream))
  ;; allow for combination encoder/not chunking/not
  (if (slot-boundp stream 'decoder)
    ;; decoded input
    (with-slots (decoder) stream
      (let ((binary-reader (stream-binary-reader (chunked-stream-stream stream))))
        (flet ((chunked-stream-character-reader (stream)
                 ;; chunked encoded output
                 (funcall decoder #'always-chunked-stream-read-byte stream))
               (unchunked-stream-character-reader (stream)
                 ;; no chunking decode direct from the wrapped stream
                 (funcall decoder binary-reader stream)))
          (if (chunked-stream-input-chunking-p stream)
              (values #'chunked-stream-character-reader stream)
              (values #'unchunked-stream-character-reader (chunked-stream-stream stream))
              ))))
    ;; binary input
    (stream-binary-reader stream)))

(defmethod stream-binary-reader ((stream stream))
  ;; just return a read-byte wrapper which transforms eof into nil
  (flet ((binary-stream-reader (stream)
           (let ((byte (read-byte stream nil nil)))
             (when (integerp byte)
               byte))))
    (values #'binary-stream-reader
            stream)))

(defmethod stream-binary-reader ((stream de.setf.utility.implementation::vector-input-stream))
  (stream-reader stream))


(defmethod stream-peek-char ((stream http:input-stream))
  (with-slots (decoder) stream
    (let ((char (funcall decoder #'chunked-stream-read-byte stream)))
      (cond (char
             (stream-unread-char stream char)
             char)
            (t
             nil)))))


;;; stream-read-byte : inherited

(defmethod stream-read-char ((stream http:input-stream))
  "Read a character from an open stream according to its current encoding.
  At EOF return nil."
  (with-slots (decoder) stream
    (funcall decoder #'chunked-stream-read-byte stream)))


(defmethod stream-read-char-no-hang ((stream http:input-stream))
  "If input is already available from an open stream read the next character according
 to its current encoding. If none is available, return NIL. At EOF return nil."
  (with-slots (body-position body-length) stream
    (when (stream-listen stream)
      (stream-read-char stream))))


(defmethod stream-read-line ((stream http:input-stream))
   "Read a line of characters from an open stream according to its current
 encoding. Return those up to the next stream-eol-marker as a new string.
 Iff the line is terminated by EOF, return a second value, t."
  (with-slots (decoder) stream
    (let ((eol-marker (stream-eol-marker stream))
          (line (stream-line-buffer stream)))
      (setf (fill-pointer line) 0)
      (loop for char = (funcall decoder #'chunked-stream-read-byte stream)
            do (cond ((eql char eol-marker)
                      (return (copy-seq line)))
                     (char
                      (vector-push-extend char line))
                     (t
                      (return (values (copy-seq line) t))))))))


(defmethod stream-read-sequence
          ((stream http:input-stream) (sequence string) start end &key)
  "Read a character sequence from an open stream, construct characters, and
  return the the next position. Iff the first byte read shows eof, return nil."
  (unless start (setf start 0))
  (setf end (or end (length sequence)))
  (with-slots (decoder) stream
    (if (> end start)
      (let ((char (funcall decoder #'chunked-stream-read-byte stream)))
        (when char
          (setf (char sequence start) char)
          (do ((i (1+ start) (1+ i)))
              ((>= i end) end)
            (if (setf char (funcall decoder #'chunked-stream-read-byte stream))
              (setf (char sequence i) char)
              (return i)))))
      end)))

(defmethod stream-read-sequence
          ((stream http:input-stream) (sequence vector) start end &key)
  "Read a character sequence from an open stream, construct characters, and
  return the the next position. Iff the first byte read shows eof, return nil."
  (unless start (setf start 0))
  (setf end (or end (length sequence)))
  (if (> end start)
    (let ((byte (chunked-stream-read-byte stream)))
        (when byte
          (setf (aref sequence start) byte)
          (do ((i (1+ start) (1+ i)))
              ((>= i end) end)
            (if (setf byte (chunked-stream-read-byte stream))
              (setf (aref sequence i) byte)
              (return i)))))
      end))


(defmethod stream-tyi ((stream http:input-stream))
  (let ((char (stream-read-char stream)))
    (typecase char
      (character char)
      (t nil))))


(defmethod stream-unread-char ((stream http:input-stream) char)
  "Push the character onto a cache and, iff the first one, swap the decoder with
 one which will respons to read-char calls by exhausting the pushed ones and the
 restoring the original
 decoder."
  (unless (rest (push char (stream-unread-characters stream)))
    (let ((old-decoder (stream-decoder stream)))
      (flet ((unread-decoder (byte-decoder stream)
               (declare (ignore byte-decoder))
               (let ((cache (stream-unread-characters stream)))
                 (prog1 (first cache)
                   (unless (setf (stream-unread-characters stream) (rest cache))
                     (setf-stream-decoder old-decoder stream))))))
        (setf-stream-decoder #'unread-decoder stream)
        nil))))


(defmethod stream-untyi ((stream http:input-stream) character)
  (stream-unread-char stream character))


;;; output

(defgeneric stream-clear-header-output (stream)
  (:documentation "Reset the header buffer stream to accept new specifications
   by retrieving its string. Return the string to indicate success, but return
   nil if the stream has been closed, as the closed state indicates that the
   headers have already been sent. If just reset, leave it open.")
  (:method ((stream http:output-stream))
    (let ((header-stream (http:stream-header-stream stream)))
      (when (open-stream-p header-stream)
        (get-output-stream-string header-stream)))))


(defgeneric stream-finish-header-output (stream)
  (:documentation "Ensure that headers have been written to the reference stream.")
  (:method ((stream http:output-stream))
    (let* ((header-stream (http:stream-header-stream stream)))
      (when (open-stream-p header-stream)
        (when (eql 0 (stream-file-position header-stream))
          (http:send-headers http:*response*))
        (let* ((header-string (get-output-stream-string header-stream))
               (reference-stream (chunked-stream-stream stream)))
          (loop for char across header-string
            for char-code = (char-code char)
            do (write-byte char-code reference-stream))
          (close header-stream)
          header-string)))))


(defgeneric stream-header-output-finished-p (stream)
  (:documentation "Return true iff the buffered header output has been written.")
  (:method ((stream http:output-stream))
    (let ((header-stream (http:stream-header-stream stream)))
      (not (open-stream-p header-stream)))))
 

(defun ensure-header-output-finished (stream)
  (unless (stream-header-output-finished-p stream)
    (stream-finish-header-output stream)))


(defmethod chunga::flush-buffer :before ((stream http:output-stream))
  "Ensure that, in the event that no output occurred, the headers are still written"
  (ensure-header-output-finished stream))
        

;; content

#+(or)                                  ; unused
(defun chunked-stream-write-byte (stream byte)
  ;; transliterated from stream-write-byte (chunked-stream)
  "Writes one byte by simply adding it to the end of the output
   buffer iff output chunking is enabled. Otherwise write through to
   the wrapped stream.
   The buffer is flushed if necessary."
  (cond ((chunked-stream-output-chunking-p stream)
         (with-slots (chunga::output-index chunga::output-buffer) stream
           (when (>= chunga::output-index chunga::+output-buffer-size+)
             (chunga::flush-buffer stream))
           (setf (aref chunga::output-buffer chunga::output-index) byte)
           (incf chunga::output-index)
           byte))
        (t
         (ensure-header-output-finished stream)
         (write-byte byte (chunked-stream-stream stream)))))


(defmethod stream-binary-writer ((stream http:output-stream))
  "Return the currently configured writer/arg combination.
 In the initial state, this ensures that any headers have been flushed"
  (values (stream-byte-writer stream)
          (stream-byte-writer-arg stream)))


(defmethod stream-binary-writer ((stream stream))
  "Make a writer available for the cases where the output is broadcast to a second stream"
  (values (if (find-method #'stream-write-byte () (list (class-of stream) (find-class t)) nil)
            #'stream-write-byte
            #'(lambda (stream byte)
                (write-byte byte stream)))
          stream))


(defmethod stream-writer ((stream http:output-stream))
  "Return the character encoding writer which combines the current character encoder and binary writer/arg"
  (values (stream-char-writer stream)
          (stream-char-writer-arg stream)))


(defmethod stream-fresh-line ((stream http:output-stream))
  (stream-write-char stream #\newline))

(defmethod stream-finish-output ((stream http:output-stream))
  "Ensure that the headers are written - for use before body is sent (see send-entity-body),
 and force the last chunk."
  (stream-force-output stream)
  (call-next-method stream)
  ;; ensure that the last block is flushed - even prior to close
  (setf (chunga:chunked-stream-output-chunking-p stream) nil))

(defmethod stream-force-output ((stream http:output-stream))
  "Ensure that the headers are written - for use before body is sent (see send-entity-body)"
  (call-next-method))

(defmethod stream-clear-output ((stream http:output-stream))
  "Reset the cached writer functions and call next."
  (initialize-stream-writers stream)
  (setf (stream-header-stream strream) (make-string-output-stream))
  (call-next-method))


(defmethod stream-terpri ((stream http:output-stream))
  (let ((marker (stream-eol-marker stream)))
    (if (stringp marker)
      (stream-write-string stream marker 0 (length marker))
      (stream-write-char stream marker))))


(defmethod stream-write-byte ((stream http:output-stream) byte)
  ;; built into initial writer (ensure-header-output-finished stream)
  (funcall (stream-byte-writer stream) (stream-byte-writer-arg stream) byte))


(defmethod stream-write-char ((stream http:output-stream) character)
  ;; built into initial writer (ensure-header-output-finished stream)
  (funcall (stream-encoder stream) character (stream-byte-writer stream) (stream-byte-writer-arg stream)))


(defmethod stream-write-string ((stream http:output-stream) (string string) #-mcl &optional start end)
  "Write a string to chunked stream according to its current encoding."
  (unless start (setf start 0))
  (unless end (setf end (length string)))
  (let ((encoder (stream-encoder stream)))
    (let ((byte-writer (stream-byte-writer stream))
          (byte-writer-arg (stream-byte-writer-arg stream)))
      (do ((i start (1+ i)))
          ((>= i end))
        (funcall encoder (char string i) byte-writer byte-writer-arg)))
    string))


(defmethod chunga::stream-write-sequence ((stream chunked-output-stream) (sequence string) #+ccl &key start end #-ccl &key)
  "Encode a character SEQUENCE by appending its content to the output buffer if it's
small enough."
  (setf end (or end (length sequence)))
  (stream-write-string stream sequence start end))


(defmethod stream-tyo ((stream chunked-output-stream) character)
  (stream-write-char stream character))

