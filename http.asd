;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: common-lisp-user; -*-
;;; Copyright 2013 [james anderson](mailto:james.anderson@setf.de) All Rights Reserved

(in-package :common-lisp-user)

;;;
;;;  This file is the system definition for http support for'org.datagraph.spocq'.
;;;
;;;  Copyright 2013 [james anderson](mailto:james.anderson@setf.de) All Rights Reserved

(setq *compile-verbose* nil)

(asdf:defsystem :de.setf.http
  :depends-on (:de.setf.utility.mime
               :closer-mop
               :cl-ppcre
               :chunga                  ; direct for the cgi version
               #+sbcl :sb-cltl2
               )
  :description
  "a modulor CLOS framework to implement HTTP with concrete implementations
   for hunchentoot and cl-fastcgi"
  :serial t
  :version "do not even think about it"
  :components ((:file "package")
               (:file "parameters")
               (:file "utilities")
               (:file "conditions")
               (:file "http")
               ))
