;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: HUNCHENTOOT; -*-
;;; Copyright (c) 2021  [james anderson](mailto:james.anderson@setf.de) All Rights Reserved

;;; modify read-http-headers logic to permit emptry headers

(in-package :chunga)

(defun read-http-headers (stream &optional log-stream)
  "Reads HTTP header lines from STREAM \(except for the initial
status line which is supposed to be read already) and returns a
corresponding alist of names and values where the names are
keywords and the values are strings.  Multiple lines with the
same name are combined into one value, the individual values
separated by commas.  Header lines which are spread across
multiple lines are recognized and treated correctly.  Additonally
logs the header lines to LOG-STREAM if it is not NIL."
  (let (headers
        (*current-error-message* "While reading HTTP headers:"))
    (labels ((read-header-line ()
               "Reads one header line, considering continuations."
               (with-output-to-string (header-line)
                 (loop
                  (let ((line (trim-whitespace (read-line* stream log-stream))))
                    (when (zerop (length line))
                      (return))
                    (write-sequence line header-line)
                    (let ((next (peek-char* stream)))
                      (unless (whitespacep next)
                        (return)))
                    ;; we've seen whitespace starting a continutation,
                    ;; so we loop
                    (write-char #\Space header-line)))))
             (split-header (line)
               "Splits line at colon and converts it into a cons.
Returns NIL if LINE consists solely of whitespace."
               (unless (zerop (length (trim-whitespace line)))
                 (let* ((colon-pos (or (position #\: line :test #'char=)
                                       (error 'syntax-error
                                              :stream stream
                                              :format-control "Couldn't find colon in header line ~S."
                                              :format-arguments (list line))))
                        (field (trim-whitespace (subseq line (1+ colon-pos)))))
                   (cons (as-keyword (subseq line 0 colon-pos))
                         (unless (zerop (length field)) field)))))
             (add-header (pair)
               "Adds the name/value cons PAIR to HEADERS.  Takes
care of multiple headers with the same name."
               (let* ((name (car pair))
                      (existing-header (assoc name headers :test #'eq))
                      (existing-value (cdr existing-header)))
                 (cond (existing-header
                        (setf (cdr existing-header)
                              (format nil "~A~:[,~;~]~A"
                                      existing-value
                                      (and *treat-semicolon-as-continuation*
                                           (eq name :set-cookie)
                                           (ends-with-p (trim-whitespace existing-value) ";"))
                                      (cdr pair))))
                       (t (push pair headers))))))
      (loop for header-pair = (split-header (read-header-line))
            while header-pair
            do (add-header header-pair)))
    (nreverse headers)))