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

(defun get-macro-character* (char &optional (readtable *readtable*))
  "Just like CL:GET-MACRO-CHARACTER but its second value defaults to T even if there's no macro fn installed on CHAR."
  (bind (((:values fn nonterminating?) (get-macro-character char readtable)))
    (unless fn
      ;; the standard sais that if fn is nil then it must be nil, too. but by default
      ;; everything is non-terminating, so switch it back.
      (setf nonterminating? t))
    (values fn nonterminating?)))

