;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :common-lisp-user)

(defpackage :hu.dwim.syntax-sugar
  (:use :hu.dwim.common-lisp)

  (:export #:cl-source-file-with-readtable
           #:system-with-readtable
           #:system-connection-with-readtable
           #:define-syntax
           #:*toplevel-readtable*
           #:*quasi-quote-lexical-depth*
           #:*quasi-quote-nesting-level*
           #:get-macro-character*
           #:with-local-readtable
           #:list-readers

           ;; export it early, we don't know in which order the integrations are loaded
           #:register-readtable-for-swank))
