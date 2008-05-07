;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(cl:in-package :cl-user)

;;; try to load asdf-system-connections
(eval-when (:compile-toplevel :load-toplevel :execute)
  (flet ((try (system)
           (unless (asdf:find-system system nil)
             (warn "Trying to install required dependency: ~S" system)
             (when (find-package :asdf-install)
               (funcall (read-from-string "asdf-install:install") system))
             (unless (asdf:find-system system nil)
               (error "The ~A system requires ~A." (or *compile-file-pathname* *load-pathname*) system)))
           (asdf:operate 'asdf:load-op system)))
    (try :asdf-system-connections)))

(defpackage #:cl-syntax-sugar-system
  (:use :cl :asdf :asdf-system-connections))

(in-package #:cl-syntax-sugar-system)

(defsystem :cl-syntax-sugar
  :author ("Levente Mészáros <levente.meszaros@gmail.com>"
           "Attila Lendvai <attila.lendvai@gmail.com>")
  :licence "BSD / Public Domain"
  :description "Syntax sugar"
  :depends-on (:alexandria :metabang-bind)
  :components
  ((:module "src"
            :components ((:file "package")
                         (:file "duplicates" :depends-on ("package"))
                         (:file "asdf-integration" :depends-on ("package" "duplicates"))
                         (:file "syntax-sugar" :depends-on ("duplicates"))
                         (:file "one-liners" :depends-on ("duplicates" "syntax-sugar"))
                         (:file "readtime-wrapper" :depends-on ("one-liners" "duplicates" "syntax-sugar"))
                         (:file "quasi-quote" :depends-on ("one-liners" "duplicates" "syntax-sugar"))))))

(defsystem :cl-syntax-sugar-unicode
  :author ("Levente Mészáros <levente.meszaros@gmail.com>"
           "Attila Lendvai <attila.lendvai@gmail.com>")
  :licence "BSD / Public Domain"
  :description "Extensions to cl-syntax-sugar using unicode characters"
  :depends-on (:cl-syntax-sugar)
  :components
  ((:module "src"
            :components ((:module "unicode-enabled"
                                  :components ((:file "package")
                                               (:file "one-liners" :depends-on ("package"))))))))

(defsystem-connection cl-syntax-sugar-and-cl-walker
  :requires (:cl-syntax-sugar :cl-walker)
  :components
  ((:module "src"
            :components ((:file "cl-walker-integration")
                         (:file "lambda" :depends-on ("cl-walker-integration"))))))

(defsystem-connection cl-syntax-sugar-and-swank
  :requires (:cl-syntax-sugar :swank)
  :components
  ((:module "src"
            :components ((:file "swank-integration")))))

(defsystem :cl-syntax-sugar-test
  :description "Tests for the cl-syntax-sugar system."
  :depends-on (:cl-syntax-sugar :cl-syntax-sugar-unicode :cl-walker :stefil)
  :components
  ((:module "tests"
            :serial t
            :components ((:file "package")
                         (:file "test-environment")
                         (:file "readtime-wrapper")
                         (:file "quasi-quote")
                         (:file "lambda")
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

