;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-user)

(defpackage #:cl-syntax-sugar-system
  (:use :cl :asdf))

(in-package #:cl-syntax-sugar-system)

(defsystem :cl-syntax-sugar
  :version "0.1"
  :author ("Levente Mészáros <levente.meszaros@gmail.com>"
           "Attila Lendvai <attila.lendvai@gmail.com>")
  :licence "BSD / Public Domain"
  :description "Syntax sugar"
  :depends-on (:alexandria :metabang-bind :iterate)
  :components
  ((:module "src"
            :components ((:file "package")
                         (:file "duplicates" :depends-on ("package"))
                         (:file "syntax-sugar" :depends-on ("duplicates"))
                         (:file "one-liners" :depends-on ("duplicates" "syntax-sugar"))
                         (:file "readtime-wrapper" :depends-on ("one-liners" "duplicates" "syntax-sugar"))
                         (:file "quasi-quote" :depends-on ("one-liners" "duplicates" "syntax-sugar"))
                         ;;(:file "lambda" :depends-on ("duplicates" "syntax-sugar"))
                         ))))

(defsystem :cl-syntax-sugar-test
  :description "Tests for the cl-syntax-sugar system."
  :depends-on (:cl-syntax-sugar :stefil)
  :components
  ((:module "tests"
            :serial t
            :components ((:file "package")
                         (:file "test-environment")
                         (:file "readtime-wrapper")
                         (:file "quasi-quote")))))

(defmethod perform ((op test-op) (system (eql (find-system :cl-syntax-sugar))))
  (operate 'load-op :cl-syntax-sugar-test)
  (in-package :cl-syntax-sugar-test)
  (declaim (optimize (debug 3)))
  (warn "(declaim (optimize (debug 3))) was issued to help later C-c C-c'ing")
  (eval (read-from-string "(progn
                             (stefil:funcall-test-with-feedback-message 'test))"))
  (values))

(defmethod operation-done-p ((op test-op) (system (eql (find-system :cl-syntax-sugar))))
  nil)

