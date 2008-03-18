;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(cl:in-package :common-lisp-user)

(defpackage :cl-syntax-sugar-test
  (:use #:common-lisp :cl-syntax-sugar :stefil :alexandria :metabang-bind :iterate)

  (:shadow
   #:deftest))
