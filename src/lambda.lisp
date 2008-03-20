;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-syntax-sugar)

(define-syntax lambda-with-bang-args (&key dispatch-character sub-dispatch-character
                                           start-character end-character)
  "Reader macro for simple lambdas.

This read macro reads exactly one form and serves to eliminate
the 'boiler' plate text from such lambdas and write only the body
of the lambda itself. If the form contains any references to
variables named !1, !2, !3, !n etc. these are bound to the Nth
parameter of the lambda.

Examples:

#L(foo) ==> (lambda () (foo)).

#L(foo !1) ==> (lambda (!1) (foo !1))

#L(foo (bar !2) !1) ==> (lambda (!1 !2) (foo (bar !2) !1))

All arguments are declared ignorable. So if there is a reference
to an argument !X but not !(x-1) we still take X arguments, but x
- 1 is ignored. Examples:

#L(foo !2) ==> (lambda (!1 !2) (declare (ignore !1)) (foo !2))

We can specify exactly how many arguments to take by using the
read macro's prefix parameter. NB: this is only neccessary if the
lambda needs to accept N arguments but only uses N - 1. Example:

#2L(foo !1) ==> (lambda (!1 !2) (declare (ignore !2)) (foo !1))

When #l forms are nested, !X variables are bound to the innermost 
form. Example:

#l#l(+ !1 !2)

returns a function that takes no arguments and returns a function
that adds its two arguments."
  (when (and (or dispatch-character sub-dispatch-character)
             (or start-character end-character))
    (error "You may either install this syntax on a dispatching macro character or on start/end characters"))
  (assert (not (xor dispatch-character sub-dispatch-character)))
  (assert (not (xor start-character end-character)))
  (if dispatch-character
      (progn
        (unless (get-macro-character dispatch-character)
          (make-dispatch-macro-character dispatch-character))
        (set-dispatch-macro-character dispatch-character sub-dispatch-character
                                      (make-lambda-with-bang-args-reader nil)))
      (progn
        (set-macro-character start-character
                             (make-lambda-with-bang-args-reader end-character)
                             t *readtable*)
        (set-syntax-from-char end-character #\) *readtable*))))

(define-syntax sharp-l ()
  "Enables the LAMBDA-WITH-BANG-ARGS syntax on #L(+ !1 !2)"
  (set-lambda-with-bang-args-syntax-in-readtable
   :dispatch-character #\#
   :sub-dispatch-character #\L))

(defun make-lambda-with-bang-args-reader (end-character)
  (if end-character
      (named-lambda lambda-with-bang-args-reader (stream char)
        (declare (ignore char))
        (bind ((body (read-delimited-list end-character stream t)))
          `(lambda-with-bang-args-expander ,(package-name *package*) ,body nil)))
      (named-lambda lambda-with-bang-args-reader (stream subchar numeric-arg)
        (declare (ignore subchar numeric-arg))
        (bind ((body (read stream t nil t)))
          `(lambda-with-bang-args-expander ,(package-name *package*) ,body nil)))))

(defmacro lambda-with-bang-args-expander (package-designator body min-args &environment env)
  (bind ((package package-designator))
    (unless (packagep package)
      (setf package (find-package package)))
    (unless package
      (error "Could not find package ~S?!" package-designator))
    (flet ((make-bang-arg (package number)
             (intern (format nil "!~D" number) package)))
      (bind ((form body)
             (lambda-args (loop
                             :for i :upfrom 1 :upto (max (or min-args 0)
                                                         (find-highest-bang-var form env))
                             :collect (make-bang-arg package i))))
        `(lambda ,lambda-args
           ,@(when lambda-args
                   `((declare (ignorable ,@lambda-args))))
           ,form)))))

(defun find-highest-bang-var (form env)
  (bind ((*warn-undefined* nil))
    (labels ((bang-var-p (form)
               (and (starts-with #\! (symbol-name form) :test #'char=)
                    (parse-integer (subseq (symbol-name form) 1) :junk-allowed t)))
             ;; TODO add a util for this kind of collecting in the walker
             (collect-var-references (input-form)
               (typecase input-form
                 (cons
                  (append (collect-var-references (car input-form))
                          (collect-var-references (cdr input-form))))

                 (free-variable-reference (list (slot-value input-form 'name)))
                 (local-lexical-variable-reference (list (slot-value input-form 'name)))
                 (form
                  (loop
                     :for slot-name :in (mapcar #'sb-mop:slot-definition-name
                                                (sb-mop:class-slots (class-of input-form)))
                     :when (not (member slot-name '(parent target-progn enclosing-tagbody target-block)))
                       :append (collect-var-references (slot-value input-form slot-name))))
                 (t nil))))
      (or (loop
             :for var :in (collect-var-references (walk-form form nil (make-walk-env env)))
             :when (bang-var-p var)
               :maximize (bang-var-p var))
          0))))
