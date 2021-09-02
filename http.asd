;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: common-lisp-user; -*-
;;; Copyright 2014 [james anderson](mailto:james.anderson@setf.de) All Rights Reserved

(in-package :common-lisp-user)

;;;
;;;  This file is the system definition for the http library 'de.setf.http'.
;;;

(setq *compile-verbose* nil)

(asdf:defsystem :de.setf.http
  :depends-on (:de.setf.utility.mime
               :de.setf.utility.codecs
               :de.setf.utility.clos
               :closer-mop
               :cl-ppcre
               :chunga                  ; direct for the cgi version
               :trivial-gray-streams    ; likewise
               #+sbcl :sb-cltl2
	       ;; asdf w/o hierarchival names :com.b9.puri.puri-ppcre  ; for user agent strings
	       :puri-ppcre  ; for user agent strings
               ;; asdf w/o :net.common-lisp.cffi    ; for syslog
	       :cffi    ; for syslog
               )
  :description
  "a modulor CLOS framework to implement HTTP with concrete implementations
   for hunchentoot and cl-fastcgi"
  :serial t
  :version "0.1.1"
  :components ((:file "package")
               (:file "parameters")
               (:file "utilities")
               (:file "conditions")
               (:file "stream")
               (:file "http")
               ))
