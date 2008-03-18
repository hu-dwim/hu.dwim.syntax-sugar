;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-syntax-sugar)

(defmacro if-bind (var test &body then/else)
  (assert (first then/else)
          (then/else)
          "IF-BIND missing THEN clause.")
  (destructuring-bind (then &optional else)
      then/else
    `(let ((,var ,test))
       (if ,var ,then ,else))))

(defmacro aif (test then &optional else)
  `(if-bind it ,test ,then ,else))

(defmacro when-bind (var test &body body)
  `(if-bind ,var ,test (progn ,@body)))

(defmacro awhen (test &body body)
  `(when-bind it ,test ,@body))

(defmacro prog1-bind (var ret &body body)
  `(let ((,var ,ret))
    ,@body
    ,var))

(defmacro aprog1 (ret &body body)
  `(prog1-bind it ,ret ,@body))

;;; Copied over from Stefil

(defmacro with-lambda-parsing ((lambda-form &key finally) &body body)
  (with-unique-names (cell)
    `(iter
      (with -in-keywords- = nil)
      (with -in-optionals- = nil)
      (with -rest-variable-name- = nil)
      (for ,cell :first ,lambda-form :then (cdr ,cell))
      (while ,cell)
      (for -variable-name- = (if (or -in-optionals-
                                     -in-keywords-)
                                 (first (ensure-list (car ,cell)))
                                 (car ,cell)))
      (for -default-value- = (if (or -in-optionals-
                                     -in-keywords-)
                                 (second (ensure-list (car ,cell)))
                                 (car ,cell)))
      (case -variable-name-
        (&optional (setf -in-optionals- t))
        (&key (setf -in-keywords- t)
              (setf -in-optionals- nil))
        (&allow-other-keys)
        (&rest (setf -rest-variable-name- (car (cdr ,cell)))
               (setf ,cell (cdr ,cell)))
        (t ,@body))
      (finally ,@finally))))

(defun lambda-list-to-funcall-list (args)
  (with-lambda-parsing (args :finally ((return (values result -rest-variable-name-))))
    (if -in-keywords-
        (progn
          (collect (intern (symbol-name (first (ensure-list -variable-name-)))
                           #.(find-package "KEYWORD")) :into result)
          (collect -variable-name- :into result))
        (collect -variable-name- :into result))))

(defun lambda-list-to-funcall-expression (function args)
  (bind (((:values arg-list rest-variable) (lambda-list-to-funcall-list args)))
    (if rest-variable
        `(apply ,function ,@arg-list ,rest-variable)
        `(funcall ,function ,@arg-list))))

(defun lambda-list-to-value-list-expression (args)
  `(list ,@(with-lambda-parsing (args)
             (collect `(cons ',-variable-name- ,-variable-name-)))))

(defun lambda-list-to-variable-list (args &key (include-defaults nil) (include-&rest nil))
  (with-lambda-parsing (args :finally ((return (if (and include-&rest
                                                        -rest-variable-name-)
                                                   (cons -rest-variable-name- result)
                                                   result))))
    (collect (if include-defaults
                 (list -variable-name- -default-value-)
                 -variable-name-)
      :into result)))

