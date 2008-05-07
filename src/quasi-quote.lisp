;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-syntax-sugar)

(defparameter *quasi-quote-nesting-level* 0)

(define-syntax quasi-quote (quasi-quote-wrapper unquote-wrapper
                                                &key
                                                (nested-quasi-quote-wrapper quasi-quote-wrapper)
                                                (start-character #\`)
                                                dispatch-character
                                                end-character
                                                (unquote-character #\,)
                                                (splice-character #\@)
                                                (toplevel-reader-wrapper #'identity)
                                                readtable-case)
  (when (and dispatch-character end-character)
    (error "You can not install on both a dispatch character and an end character"))
  (bind ((reader (make-quasi-quote-reader start-character end-character
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

(defun make-quasi-quote-reader (start-character end-character
                                quasi-quote-wrapper nested-quasi-quote-wrapper
                                unquote-character unquote-wrapper
                                splice-character
                                toplevel-reader-wrapper
                                readtable-case)
  (labels ((toplevel-quasi-quote-reader (stream &optional char1 char2)
             (declare (ignore char1 char2))
             (bind ((entering-readtable *readtable*)
                    (entering-quasi-quote-nesting-level *quasi-quote-nesting-level*)
                    (*quasi-quote-nesting-level* (1+ *quasi-quote-nesting-level*))
                    (*toplevel-readtable* (or *toplevel-readtable* *readtable*))
                    (*readtable* (copy-readtable)))
               (set-macro-character unquote-character (make-unquote-reader entering-quasi-quote-nesting-level entering-readtable))
               (bind ((original-sharp-dot-reader (get-dispatch-macro-character #\# #\.)))
                 ;; make sure #. reader is called in the respored readtable-case.
                 ;; TODO this should be generalized to wrap all the readers installed in the readtable? probably it's not worth the trouble...
                 (set-dispatch-macro-character #\# #\. (lambda (&rest args)
                                                         (bind ((*readtable* (copy-readtable)))
                                                           (setf (readtable-case *readtable*) (readtable-case *toplevel-readtable*))
                                                           (apply original-sharp-dot-reader args)))))
               (when end-character
                 (set-macro-character start-character (make-nested-quasi-quote-reader entering-quasi-quote-nesting-level)))
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
                     (list quasi-quote-wrapper body)))))
           (make-nested-quasi-quote-reader (entering-quasi-quote-nesting-level)
             (named-lambda nested-quasi-quote-reader (stream char)
               (declare (ignore char))
               (assert (>= *quasi-quote-nesting-level* entering-quasi-quote-nesting-level))
               (bind ((body (if end-character
                                (read-delimited-list end-character stream t)
                                (read stream t nil t))))
                 (if (functionp nested-quasi-quote-wrapper)
                     (funcall nested-quasi-quote-wrapper body)
                     (list nested-quasi-quote-wrapper body)))))
           (make-unquote-reader (entering-quasi-quote-nesting-level entering-readtable)
             (named-lambda unquote-reader (stream char)
               (declare (ignore char))
               (bind ((*readtable* (copy-readtable)) ; this is only needed when actually restoring the original readers, but then it's needed inside the recursive READ call down at the end, so wrap it all up with a copy
                      (*quasi-quote-nesting-level* (1- *quasi-quote-nesting-level*))
                      (spliced? (eq (peek-char nil stream t nil t) splice-character)))
                 (when spliced?
                   (read-char stream t nil t))
                 (assert (>= *quasi-quote-nesting-level* entering-quasi-quote-nesting-level))
                 (when (= *quasi-quote-nesting-level* entering-quasi-quote-nesting-level)
                   ;; restore the original readers when we are leaving our nesting. this way it's possible
                   ;; to use the ` and , in their normal meanings when being outside our own nesting levels.
                   (setf (readtable-case *readtable*) (readtable-case *toplevel-readtable*))
                   (apply 'set-macro-character unquote-character (multiple-value-list (get-macro-character* unquote-character entering-readtable)))
                   ;; we don't restore the reader on the end character, if there was any, because that
                   ;; would break things like [a ,b] due to a #\] being nonterminating by default.
                   (set-macro-character start-character (funcall toplevel-reader-wrapper #'toplevel-quasi-quote-reader)))
                 (bind ((body (read stream t nil t)))
                   (if (functionp unquote-wrapper)
                       (funcall unquote-wrapper body spliced?)
                       (list unquote-wrapper body spliced?)))))))
    (funcall toplevel-reader-wrapper #'toplevel-quasi-quote-reader)))
