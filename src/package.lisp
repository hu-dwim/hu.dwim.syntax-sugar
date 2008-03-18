;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(cl:in-package :cl-user)

(defpackage :cl-syntax-sugar
  (:use #:common-lisp :alexandria :metabang-bind :iterate)

  (:export #:Λ
           #:λ
           #:α
           #:β
           #:γ
           #:δ
           #:ε
           #:Σ
           #:Π))
