;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.http.implementation; -*-
;;; Copyright 2013 [james anderson](mailto:james.anderson@setf.de) All Rights Reserved

(in-package :de.setf.http.implementation)

(:documentation "chunked streams with codecs"
 "An HTTP stream has limited, well-defined tasks, for which layering and optional extensions
 add little to the performance. Instead of using the structure

   (write (encode object external-format) (chunking (stream socket)))

 structure - with the analogous read mechanism, this implementation adopts the simpler form

   (write object (chunkin+encoding (stream socket)))

 whereby, there is no reason to not ultimately integrate the chunking+encoding mechanism directly
 in the socket stream, should the overhaed be prohibitive, but that is likely worthwhile only
 if one bothers to work directly with the network buffers.

 much of the implementation is adapted from the de.setf.amqp streams.")


(defclass http:stream (stream)
  ((media-type
    :initform (make-instance 'mime:text/plain :charset :utf-8)
    :reader stream-media-type :writer setf-stream-media-type)
   (eof-marker
    :initform :eof :initarg :eof-marker
    :accessor stream-eof-marker)
   (eol-marker
    :initform #\newline :initarg :eol-marker
    :accessor stream-eol-marker)))

(defclass http:input-stream (http:stream chunga:chunked-input-stream)
  ((decoder
    :type function
    :reader  stream-decoder :writer setf-device-decoder
    :documentation "Optionally binds a function which is then used to decode
 character values from input from the device.")
   (unread-characters
    :initform ()
    :accessor stream-unread-characters))

(defclass http:output-stream (http:stream chunga:chunked-output-stream)
  ((encoder
    :type function
    :reader  device-encoder :writer setf-device-encoder
    :documentation "Optionally binds a function which is then used to encode
 character values for output to the device.")))


(defmethod stream-direction ((stream http:output-stream))
  :output)

(defmethod stream-direction ((stream http:input-stream))
  :input)

(defmethod stream-element-type ((stream http:stream))
  "Return the element type corresponding to the current media type"
  (if (mime:binary-mime-type-p (stream-media-type stream))
    '(unsigned-byte 8)
    'character))


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
         (if (or (chunked-input-available-p stream)
                 (fill-buffer stream))
           (with-slots (chunga::input-buffer chunga::input-index) stream
             (prog1 (aref chunga::input-buffer chunga::input-index)
               (incf chunga::input-index)))
           (stream-eof-marker stream)))
        ((read-byte (chunked-stream-stream stream) nil nil))
        (t
         (stream-eof-marker stream))))



(defmethod stream-peek-char ((stream http:input-stream))
  (with-slots (decoder) stream
    (let ((char (funcall decoder #'chunked-stream-read-byte stream)))
      (cond (char
             (stream-unread-char stream char)
             char)
            (t
             (stream-eof-marker stream))))))


;;; stream-read-byte : inherited

(defmethod stream-read-char ((stream http:input-stream))
  "Read a character from an open channel on an amqp-device according to the channel's current encoding.
  At EOF return the stream-eof-marker."
  (with-slots (decoder) stream
    (or (funcall decoder #'chunked-stream-read-byte stream) (stream-eof-marker stream))))


(defmethod stream-read-char-no-hang ((stream http:input-stream))
  "If input is already available from an open channel on an amqp-device read the next character according
 to the channel's current encoding. If none is available, return NIL. At EOF return the stream-eof-marker."
  (with-slots (body-position body-length) stream
    (if (stream-listen stream)
      (stream-read-char stream)
      (stream-eof-marker stream))))


(defmethod stream-read-line ((stream http:input-stream))
   "Read a line of characters from an open stream according to its current
 encoding. Return those up to the next stream-eol-marker as a new string.
 Iff the line is terminated by EOF, return a second value, the
 stream-eof-marker."
  (with-slots (decoder) stream
    (let ((eol-char (stream-eol-marker stream))
          (line (stream-line-buffer stream)))
      (setf (fill-pointer line) 0)
      (loop for char = (funcall decoder #'chunked-stream-read-byte stream)
            do (cond ((eql char eol-char)
                      (return (copy-seq line)))
                     (char
                      (vector-push-extend char line))
                     (t
                      (return (values (copy-seq line) (stream-eof-marker stream)))))))))


(defmethod stream-read-sequence
          ((stream http:input-stream) (sequence string) (start 0) end &key)
  "Read a character sequence from an open stream, construct characters, and
  return the the next position. Iff the first byte read shows eof, return nil."
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
                     (setf-device-decoder old-decoder stream))))))
        (setf-device-decoder #'unread-decoder stream)
        nil))))


(defmethod stream-untyi ((stream http:input-stream) character)
  (stream-unread-char stream character))


;;; output

(defun chunked-stream-write-byte ((stream http:output-stream) byte)
  ;; transliterated from stream-write-byte (chunked-stream)
  "Writes one byte by simply adding it to the end of the output
buffer iff output chunking is enabled.  The buffer is flushed
if necessary."
  (if (chunked-stream-output-chunking-p stream)
    (with-slots (chunga::output-index chunga::output-buffer) stream
      (when (>= chunga::output-index chunga::+output-buffer-size+)
        (flush-buffer stream))
      (setf (aref chunga::output-buffer chunga::output-index) byte)
      (incf chunga::output-index)
      byte)
    (write-byte byte (chunked-stream-stream stream))))


;;; stream-fresh-line : inherited
;;; default suffices ?


;;; stream-finish-output : inherited


;;; stream-force-output : inherited


;;; stream-clear-output :inherited


(defmethod stream-terpri ((stream http:output-stream))
  (stream-write-char stream (stream-eol-marker stream)))


(defmethod stream-write-char ((stream http:output-stream) character)
  (with-slots (encoder) stream
    (funcall encoder character #'chunked-stream-write-byte stream)
    character))


(defmethod stream-write-string ((stream http:output-stream) string #-mcl &optional start end)
  "Write a string to chunked stream according to its current encoding."
  (with-slots (encoder) stream
    (unless start (setf start 0))
    (unless end (setf end (length string)))
    (do ((i start (1+ i)))
        ((>= i end))
      (funcall encoder (char string i) #'chunked-stream-write-byte stream)))
  string)


(defmethod chunga::stream-write-sequence ((stream chunked-output-stream) (sequence string) #+ccl &key start end #-ccl &key)
  "Encode a character SEQUENCE by appending its content to the output buffer if it's
small enough."
  (setf end (or end (length sequence)))
  (stream-write-string stream sequence start end))


(defmethod stream-tyo ((stream chunked-output-stream) character)
  (stream-write-char stream character))
