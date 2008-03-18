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
                         ;;(:file "lambda" :depends-on ("duplicates" "syntax-sugar"))
                         ))))
