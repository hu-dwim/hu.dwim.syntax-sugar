;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-syntax-sugar)

(defvar *quasi-quote-level*)

(define-syntax quasi-quote (quasi-quote-wrapper unquote-wrapper &key
                                                (quasi-quote-character #\`)
                                                (quasi-quote-end-character nil)
                                                (unquote-character #\,)
                                                (splice-character #\@))
  (bind ((original-reader-on-quasi-quote-character (multiple-value-list (get-macro-character quasi-quote-character *readtable*)))
         (original-reader-on-unquote-character     (multiple-value-list (get-macro-character unquote-character *readtable*))))
    (set-macro-character quasi-quote-character
                         (make-quasi-quote-reader original-reader-on-quasi-quote-character
                                                  original-reader-on-unquote-character
                                                  quasi-quote-character quasi-quote-end-character quasi-quote-wrapper
                                                  unquote-character unquote-wrapper
                                                  splice-character)
                         t
                         *readtable*)))

(defun make-quasi-quote-reader (original-reader-on-quasi-quote-character
                                original-reader-on-unquote-character
                                quasi-quote-character quasi-quote-end-character quasi-quote-wrapper
                                unquote-character unquote-wrapper
                                splice-character)
  (labels ((unquote-reader (stream char)
             (declare (ignore char))
             (bind ((spliced (eq (peek-char nil stream t nil t) splice-character)))
               (when spliced
                 (read-char stream t nil t))
               `(,unquote-wrapper
                  ,(bind ((*readtable* (copy-readtable))
                          (*quasi-quote-level* (1- *quasi-quote-level*)))
                     (assert (<= 0 *quasi-quote-level*))
                     (when (zerop *quasi-quote-level*)
                       ;; restore the original readers when we are leaving our nesting. this way it's possible
                       ;; to use the ` and , in their normal meanings when being outside our own nesting levels.
                       (apply 'set-macro-character quasi-quote-character original-reader-on-quasi-quote-character)
                       (apply 'set-macro-character unquote-character original-reader-on-unquote-character))
                     (read stream t nil t))
                  ,spliced)))
           (quasi-quote-reader (stream char)
             (declare (ignore char))
             (bind ((*quasi-quote-level* (if (boundp '*quasi-quote-level*)
                                             (1+ *quasi-quote-level*)
                                             1))
                    (*readtable* (copy-readtable)))
               (set-macro-character unquote-character #'unquote-reader)
               (bind ((body (if quasi-quote-end-character
                                ;; we must set the syntax on the end char to be like #\)
                                ;; until we read out our entire body. this is needed to
                                ;; make "[... 5] style inputs work.
                                (bind ((original-reader-on-quasi-quote-end-character
                                        (multiple-value-list
                                         (get-macro-character quasi-quote-end-character *readtable*))))
                                  (unwind-protect
                                       (progn
                                         (set-syntax-from-char quasi-quote-end-character #\) *readtable*)
                                         (read-delimited-list quasi-quote-end-character stream t))
                                    (apply 'set-macro-character quasi-quote-character original-reader-on-quasi-quote-end-character)))
                                (read stream t nil t))))
                 `(,quasi-quote-wrapper ,body)))))
    #'quasi-quote-reader))

