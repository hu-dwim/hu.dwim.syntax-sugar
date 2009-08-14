;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :common-lisp-user)

(defpackage :hu.dwim.syntax-sugar.test
  (:use :hu.dwim.common-lisp
        :hu.dwim.def
        :hu.dwim.stefil
        :hu.dwim.syntax-sugar
        :hu.dwim.syntax-sugar.unicode
        :hu.dwim.walker)

  (:shadow #:deftest))
