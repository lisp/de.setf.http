;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: common-lisp-user; -*-
;;; Copyright 2013 [james anderson](mailto:james.anderson@setf.de) All Rights Reserved

(in-package :common-lisp-user)

;;;
;;;  Copyright 2013 [james anderson](mailto:james.anderson@setf.de) All Rights Reserved

(setq *compile-verbose* nil)

(asdf:defsystem :de.setf.http.hunchentoot
  :depends-on (:de.setf.http
               :hunchentoot
               :drakma
               )
  :description
  "Integrates hunchenoot as the concrete implementation for de.setf.http"
  :serial t
  :components ((:file "hunchentoot")
               (:file "chunga")
               ))
