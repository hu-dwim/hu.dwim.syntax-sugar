;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.syntax-sugar)

;; the defaults, #xFF62 and #xFF63 are ｢ and ｣
(define-syntax string-quote (&key (start-character #.(code-char #xFF62)) (end-character #.(code-char #xFF63)) (transformer #'identity))
  "A simple string quote that unconditionally reads all characters until END-CHARACTER into a string."
  (bind ((reader (make-string-quote-reader end-character transformer))
         (transformer (or transformer #'identity)))
    (flet ((string-quote-reader (stream &optional char)
             (declare (ignore char))
             (%read-quoted-string stream end-character transformer)))
      (set-macro-character start-character reader t *readtable*))))

(defun %read-quoted-string (stream end-character transformer)
  (check-type transformer (or function symbol))
  (bind ((*toplevel-readtable* (or *toplevel-readtable* *readtable*)))
    (loop
      :with result = (make-array 8 :element-type 'character :adjustable t :fill-pointer 0)
      :with base-char? = t
      :for char = (read-char stream t nil t)
      :until (char= char end-character)
      :do (progn
            (setf base-char? (and base-char?
                                  (typep char 'base-char)))
            (vector-push-extend char result))
      :finally (return (funcall transformer (if base-char?
                                                (coerce result 'simple-base-string)
                                                result))))))
