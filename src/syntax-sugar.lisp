;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-syntax-sugar)

(defmacro define-syntax (&whole whole name args &body body)
  (bind (((name &key readtime-wrapper-result-transformer) (ensure-list name))
         (enabler-name (format-symbol *package* "ENABLE-~A-SYNTAX" name))
         (enabler-function-name (format-symbol *package* "SET-~A-SYNTAX-IN-READTABLE" name))
         (readtime-wrapper-name (format-symbol *package* "WITH-~A-SYNTAX" name))
         ((:values body declarations documentation) (parse-body body :documentation t :whole whole)))
    (assert (null declarations))
    (once-only (readtime-wrapper-result-transformer)
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
           (named-lambda ,readtime-wrapper-name (handler)
             (,enabler-function-name ,@(lambda-list-to-funcall-list args))
             (bind ((result (funcall handler)))
               (if ,readtime-wrapper-result-transformer
                   (funcall ,readtime-wrapper-result-transformer result)
                   (if (rest result)
                       `(progn
                          ,@result)
                       (first result))))))
         (export '(,enabler-name ,enabler-function-name ,readtime-wrapper-name))))))

(defun Σ (list)
  (reduce #'+ list))

(defun Π (list)
  (reduce #'* list))

(defmacro √ (number)
  `(sqrt ,number))

(defmacro ∛ (number)
  `(expt ,number 1/3))

(defmacro ∜ (number)
  `(expt ,number 1/4))
