;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(cl:in-package :cl-user)

(defpackage :cl-syntax-sugar
  (:use #:common-lisp :alexandria :metabang-bind :iterate)

  (:export
   #:cl-source-file-with-readtable
   #:system-with-readtable

   #:Λ
   #:λ
   #:α
   #:β
   #:γ
   #:δ
   #:ε
   #:Σ
   #:Π))
