;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-syntax-sugar)

(define-syntax quasi-quote (quasi-quote-wrapper unquote-wrapper quasi-quote-nesting-level-variable
                                                &key
                                                (nested-quasi-quote-wrapper quasi-quote-wrapper)
                                                (start-character #\`)
                                                dispatch-character
                                                end-character
                                                (unquote-character #\,)
                                                (splice-character #\@)
                                                (toplevel-reader-wrapper #'identity)
                                                readtable-case)
  (assert quasi-quote-nesting-level-variable)
  (when (and dispatch-character end-character)
    (error "You can not install on both a dispatch character and an end character"))
  (bind ((original-reader-on-end-character (when end-character
                                             (multiple-value-list (get-macro-character end-character *readtable*))))
         (original-reader-on-unquote-character (multiple-value-list (get-macro-character unquote-character *readtable*)))
         (reader (make-quasi-quote-reader quasi-quote-nesting-level-variable
                                          original-reader-on-end-character
                                          original-reader-on-unquote-character
                                          start-character end-character
                                          quasi-quote-wrapper nested-quasi-quote-wrapper
                                          unquote-character unquote-wrapper
                                          splice-character
                                          toplevel-reader-wrapper
                                          readtable-case)))
    (if dispatch-character
        (progn
          (unless (get-macro-character dispatch-character)
            (make-dispatch-macro-character dispatch-character))
          (set-dispatch-macro-character dispatch-character start-character reader *readtable*))
        (set-macro-character start-character reader t *readtable*))))

(defun make-quasi-quote-reader (quasi-quote-nesting-level-variable
                                original-reader-on-end-character
                                original-reader-on-unquote-character
                                start-character end-character
                                quasi-quote-wrapper nested-quasi-quote-wrapper
                                unquote-character unquote-wrapper
                                splice-character
                                toplevel-reader-wrapper
                                readtable-case)
  (labels ((unquote-reader (stream char)
             (declare (ignore char))
             (progv
                 (list quasi-quote-nesting-level-variable)
                 (list (1- (symbol-value quasi-quote-nesting-level-variable)))
               (bind ((*readtable* (copy-readtable)) ; this is only needed when (zerop nesting-level) but then it's needed inside the recursive read call down below
                      (nesting-level (symbol-value quasi-quote-nesting-level-variable))
                      (spliced? (eq (peek-char nil stream t nil t) splice-character)))
                 (when spliced?
                   (read-char stream t nil t))
                 (assert (>= nesting-level 0))
                 (when (zerop nesting-level)
                   ;; restore the original readers when we are leaving our nesting. this way it's possible
                   ;; to use the ` and , in their normal meanings when being outside our own nesting levels.
                   (setf (readtable-case *readtable*) (readtable-case *toplevel-readtable*))
                   (apply 'set-macro-character unquote-character original-reader-on-unquote-character)
                   (when end-character
                     (apply 'set-macro-character end-character original-reader-on-end-character))
                   (set-macro-character start-character (funcall toplevel-reader-wrapper #'toplevel-quasi-quote-reader)))
                 (bind ((body (read stream t nil t)))
                   (if (functionp unquote-wrapper)
                       (funcall unquote-wrapper body spliced?)
                       (list unquote-wrapper body spliced?))))))
           (toplevel-quasi-quote-reader (stream &optional char1 char2)
             (declare (ignore char1 char2))
             (progv
                 (list quasi-quote-nesting-level-variable)
                 (list (if (boundp quasi-quote-nesting-level-variable)
                           (1+ (symbol-value quasi-quote-nesting-level-variable))
                           1))
               (bind ((*toplevel-readtable* (or *toplevel-readtable* *readtable*))
                      (*readtable* (copy-readtable)))
                 (set-macro-character unquote-character #'unquote-reader)
                 (set-macro-character start-character #'nested-quasi-quote-reader)
                 (when readtable-case
                   (setf (readtable-case *readtable*) readtable-case))
                 (bind ((body (if end-character
                                  ;; we must set the syntax on the end char to be like #\)
                                  ;; until we read out our entire body. this is needed to
                                  ;; make "[... 5] style inputs work where '5' is not
                                  ;; separated from ']'.
                                  (bind ((*readtable* (copy-readtable)))
                                    (set-syntax-from-char end-character #\) *readtable*)
                                    (read-delimited-list end-character stream t))
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
               (bind ((body (if end-character
                                (read-delimited-list end-character stream t)
                                (read stream t nil t))))
                 (if (functionp nested-quasi-quote-wrapper)
                     (funcall nested-quasi-quote-wrapper body)
                     (list nested-quasi-quote-wrapper body))))))
    (funcall toplevel-reader-wrapper #'toplevel-quasi-quote-reader)))
