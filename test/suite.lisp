;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.syntax-sugar.test)

(defun setup-readtable ()
  ;; these are the readers that we enable in the toplevel test package.
  (enable-sharp-l-syntax)
  (enable-readtime-wrapper-syntax)
  (enable-string-quote-syntax))

(register-readtable-for-swank
 '(:hu.dwim.syntax-sugar.test) 'setup-readtable)

(defsuite* (test :in root-suite))

(defmacro deftest (name args &body body)
  `(hu.dwim.stefil:deftest ,name ,args
     ;; it's not strictly necessary because Stefil rebinds and copies *readtable* but let's
     ;; just don't test Stefil itself and make sure we have a cloned readtable.
     (with-local-readtable
       ;; it's pretty much needed for anything we do in the tests
       (enable-readtime-wrapper-syntax)
       ;; Stefil captures the value of *package* at compile time and binds it when running
       ;; the tests, but again, let's just not test Stefil here...
       (bind ((*package* (find-package :hu.dwim.syntax-sugar.test)))
         ,@body))))
