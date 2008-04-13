;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-syntax-sugar)

(define-syntax sharp-boolean ()
  "This syntax reads \"#t\" as COMMON-LISP:T and \"#f\" as COMMON-LISP:NIL"
  (set-dispatch-macro-character #\# #\t (constantly t))
  (set-dispatch-macro-character #\# #\f (constantly nil)))

(defmacro λ (args &body body)
  "Macro on the λ unicode character expanding to CL:LAMBDA and ignoring arguments named by a single '_' character."
  (bind ((ignored-symbols (list))
         (modified-args (mapcar (lambda (el)
                                  (if (string-equal '_ el)
                                      (let ((el (gensym)))
                                        (push el ignored-symbols)
                                        el)
                                      el))
                                args)))
    `(lambda ,modified-args
       ,@(when ignored-symbols
           `((declare (ignore ,@ignored-symbols))))
       ,@body)))
