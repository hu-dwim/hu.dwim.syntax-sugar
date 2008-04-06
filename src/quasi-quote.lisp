;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-syntax-sugar)

(define-syntax quasi-quote (quasi-quote-wrapper unquote-wrapper quasi-quote-nesting-level-variable
                                                &key
                                                (nested-quasi-quote-wrapper quasi-quote-wrapper)
                                                (quasi-quote-character #\`)
                                                (quasi-quote-end-character nil)
                                                (unquote-character #\,)
                                                (splice-character #\@)
                                                (toplevel-reader-wrapper #'identity))
  (assert quasi-quote-nesting-level-variable)
  (bind ((original-reader-on-quasi-quote-end-character (when quasi-quote-end-character
                                                         (multiple-value-list (get-macro-character quasi-quote-end-character *readtable*))))
         (original-reader-on-unquote-character         (multiple-value-list (get-macro-character unquote-character *readtable*))))
    (set-macro-character quasi-quote-character
                         (make-quasi-quote-reader quasi-quote-nesting-level-variable
                                                  original-reader-on-quasi-quote-end-character
                                                  original-reader-on-unquote-character
                                                  quasi-quote-character quasi-quote-end-character
                                                  quasi-quote-wrapper nested-quasi-quote-wrapper
                                                  unquote-character unquote-wrapper
                                                  splice-character
                                                  toplevel-reader-wrapper)
                         t
                         *readtable*)))

(defun make-quasi-quote-reader (quasi-quote-nesting-level-variable
                                original-reader-on-quasi-quote-end-character
                                original-reader-on-unquote-character
                                quasi-quote-character quasi-quote-end-character
                                quasi-quote-wrapper nested-quasi-quote-wrapper
                                unquote-character unquote-wrapper
                                splice-character
                                toplevel-reader-wrapper)
  (labels ((unquote-reader (stream char)
             (declare (ignore char))
             (progv
                 (list quasi-quote-nesting-level-variable)
                 (list (1- (symbol-value quasi-quote-nesting-level-variable)))
               (bind ((*readtable* (copy-readtable))
                      (nesting-level (symbol-value quasi-quote-nesting-level-variable))
                      (spliced? (eq (peek-char nil stream t nil t) splice-character)))
                 (when spliced?
                   (read-char stream t nil t))
                 (assert (>= nesting-level 0))
                 (when (zerop nesting-level)
                   ;; restore the original readers when we are leaving our nesting. this way it's possible
                   ;; to use the ` and , in their normal meanings when being outside our own nesting levels.
                   (apply 'set-macro-character unquote-character original-reader-on-unquote-character)
                   (when quasi-quote-end-character
                     (apply 'set-macro-character quasi-quote-end-character original-reader-on-quasi-quote-end-character))
                   (set-macro-character quasi-quote-character (funcall toplevel-reader-wrapper #'toplevel-quasi-quote-reader)))
                 (bind ((body (read stream t nil t)))
                   (if (functionp unquote-wrapper)
                       (funcall unquote-wrapper body spliced?)
                       (list unquote-wrapper body spliced?))))))
           (toplevel-quasi-quote-reader (stream char)
             (declare (ignore char))
             (progv
                 (list quasi-quote-nesting-level-variable)
                 (list (if (boundp quasi-quote-nesting-level-variable)
                           (1+ (symbol-value quasi-quote-nesting-level-variable))
                           1))
                 (bind ((*readtable* (copy-readtable)))
                   (set-macro-character unquote-character #'unquote-reader)
                   (set-macro-character quasi-quote-character #'nested-quasi-quote-reader)
                   (bind ((body (if quasi-quote-end-character
                                    ;; we must set the syntax on the end char to be like #\)
                                    ;; until we read out our entire body. this is needed to
                                    ;; make "[... 5] style inputs work where '5' is not
                                    ;; separated from ']'.
                                    (bind ((*readtable* (copy-readtable)))
                                      (set-syntax-from-char quasi-quote-end-character #\) *readtable*)
                                      (read-delimited-list quasi-quote-end-character stream t))
                                    (read stream t nil t))))
                     (if (functionp quasi-quote-wrapper)
                         (funcall quasi-quote-wrapper body)
                         (list quasi-quote-wrapper body))))))
           (nested-quasi-quote-reader (stream char)
             (declare (ignore char))
             (assert (>= (symbol-value quasi-quote-nesting-level-variable) 1))
             (progv
                 (list quasi-quote-nesting-level-variable)
                 (list (1+ (symbol-value quasi-quote-nesting-level-variable)))
               (bind ((body (if quasi-quote-end-character
                                (read-delimited-list quasi-quote-end-character stream t)
                                (read stream t nil t))))
                 (if (functionp nested-quasi-quote-wrapper)
                     (funcall nested-quasi-quote-wrapper body)
                     (list nested-quasi-quote-wrapper body))))))
    (funcall toplevel-reader-wrapper #'toplevel-quasi-quote-reader)))
