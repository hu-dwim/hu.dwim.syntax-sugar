;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(cl:in-package :cl-user)

;;; try to load asdf-system-connections
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (asdf:find-system :asdf-system-connections nil)
    (when (find-package :asdf-install)
      (eval (read-from-string "(asdf-install:install '#:asdf-system-connections)")))
    (unless (asdf:find-system :asdf-system-connections nil)
      (error "The cl-syntax-sugar system requires asdf-system-connections. See http://www.cliki.net/asdf-system-connections for details and download instructions.")))
  (asdf:operate 'asdf:load-op :asdf-system-connections))

(defpackage #:cl-syntax-sugar-system
  (:use :cl :asdf :asdf-system-connections))

(in-package #:cl-syntax-sugar-system)

(defsystem :cl-syntax-sugar
  :version "0.1"
  :author ("Levente Mészáros <levente.meszaros@gmail.com>"
           "Attila Lendvai <attila.lendvai@gmail.com>")
  :licence "BSD / Public Domain"
  :description "Syntax sugar"
  :depends-on (:alexandria :metabang-bind)
  :components
  ((:module "src"
            :components ((:file "package")
                         (:file "duplicates" :depends-on ("package"))
                         (:file "integration" :depends-on ("package" "duplicates"))
                         (:file "syntax-sugar" :depends-on ("duplicates"))
                         (:file "one-liners" :depends-on ("duplicates" "syntax-sugar"))
                         (:file "readtime-wrapper" :depends-on ("one-liners" "duplicates" "syntax-sugar"))
                         (:file "quasi-quote" :depends-on ("one-liners" "duplicates" "syntax-sugar"))))))

(defsystem-connection cl-syntax-sugar-and-cl-walker
  :requires (:cl-syntax-sugar :cl-walker)
  :components
  ((:module "src"
            :components ((:file "cl-walker-integration")
                         (:file "lambda" :depends-on ("cl-walker-integration"))))))

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

(defsystem-connection cl-syntax-sugar-test-and-cl-walker
  :requires (:cl-syntax-sugar-test :cl-walker)
  :components
  ((:module "tests"
            :components ((:file "lambda")
                         (:file "sharp-l" :depends-on ("lambda"))))))

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

