;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-syntax-sugar)

(defmacro define-syntax (&whole whole name args &body body)
  (bind ((enabler-name (format-symbol *package* "ENABLE-~A-SYNTAX" name))
         (enabler-function-name (format-symbol *package* "SET-~A-SYNTAX-IN-READTABLE" name))
         (readtime-wrapper-name (format-symbol *package* "WITH-~A-SYNTAX" name))
         ((:values body declarations documentation) (parse-body body :documentation t :whole whole)))
    (assert (null declarations))
    `(progn
       (defmacro ,enabler-name ,args
         ,documentation
         `(eval-when (:compile-toplevel :execute)
            (setf *readtable* (copy-readtable *readtable*))
            (,',enabler-function-name ,,@(lambda-list-to-funcall-list args))
            (values)))
       (defun ,enabler-function-name ,args
         ,@body
         (values))
       (defun ,readtime-wrapper-name ,args
         (lambda (handler)
           (,enabler-function-name ,@(lambda-list-to-funcall-list args))
           `(progn
              ,@(funcall handler))))
       (export '(,enabler-name ,enabler-function-name ,readtime-wrapper-name)))))

(defun Σ (list)
  (reduce #'+ list))

(defun Π (list)
  (reduce #'* list))

(defmacro λ (args &body forms)
  `(lambda ,args
     ,@forms))

