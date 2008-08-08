;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-syntax-sugar)

(defparameter *quasi-quote-depth* 0
  "The absolute level of read-time (lexical) nesting of the quasi-quote readers. It does not decrease at unquotes.")

(defparameter *quasi-quote-nesting-level* 0
  "The read-time (lexical) nesting level of the quasi-quote readers, decreases when going through unquotes.")

(define-syntax quasi-quote (quasi-quote-wrapper unquote-wrapper
                                                &key
                                                (nested-quasi-quote-wrapper quasi-quote-wrapper)
                                                start-character
                                                dispatch-character
                                                end-character
                                                (unquote-character #\,)
                                                (splice-character #\@)
                                                dispatched-quasi-quote-name
                                                (toplevel-reader-wrapper #'identity)
                                                ;; body-reader is called with the stream to read the quasi quoted body
                                                ;; when it's actually time to read the body. by default the normal
                                                ;; lisp reader is called (with possible readtable customizations).
                                                body-reader
                                                readtable-case)
  (when (and dispatch-character end-character)
    (error "You can not install on both a dispatch character and an end character"))
  (when (and dispatched-quasi-quote-name
             (eql start-character #\`))
    (error "You requested the dispatched-quasi-quote reader, which is registered on the #\` character, and also provided #\` to be the start-character. Which one should win?"))
  (bind ((reader (make-quasi-quote-reader nil nil nil
                                          start-character end-character
                                          quasi-quote-wrapper nested-quasi-quote-wrapper
                                          unquote-character unquote-wrapper
                                          splice-character
                                          toplevel-reader-wrapper
                                          body-reader
                                          readtable-case)))
    (cond
      ((and dispatch-character
            start-character)
       (unless (get-macro-character dispatch-character)
         (make-dispatch-macro-character dispatch-character))
       (set-dispatch-macro-character dispatch-character start-character reader *readtable*))
      ((not (null start-character))
       (set-macro-character start-character reader t *readtable*))))
  (when dispatched-quasi-quote-name
    (bind ((previous-reader-on-backtick (get-macro-character #\`))
           (previous-reader-on-comma (get-macro-character #\,))
           (dispatching-reader (make-quasi-quote-reader dispatched-quasi-quote-name
                                                        previous-reader-on-backtick
                                                        previous-reader-on-comma
                                                        #\` nil
                                                        quasi-quote-wrapper nested-quasi-quote-wrapper
                                                        unquote-character unquote-wrapper
                                                        splice-character
                                                        toplevel-reader-wrapper
                                                        body-reader
                                                        readtable-case)))
      (set-macro-character #\` dispatching-reader t *readtable*))))

(defparameter *dispatched-quasi-quote-name* nil)

(defun make-quasi-quote-reader (dispatched-quasi-quote-name
                                previous-reader-on-backtick
                                previous-reader-on-comma
                                start-character end-character
                                quasi-quote-wrapper nested-quasi-quote-wrapper
                                unquote-character unquote-wrapper
                                splice-character
                                toplevel-reader-wrapper
                                body-reader
                                readtable-case)
  (when (and dispatched-quasi-quote-name
             end-character)
    (error "What?! dispatched-quasi-quote is always installed on #\` and should not have an end-character..."))
  (bind ((dispatched? (not (null dispatched-quasi-quote-name))))
    (labels ((toplevel-quasi-quote-reader (stream &optional char1 char2)
               (declare (ignore char1 char2))
               (read-quasi-quote nil stream))
             (read-using-original-reader (stream char)
               (if previous-reader-on-backtick
                   (bind ((*readtable* (copy-readtable)))
                     (set-macro-character #\, previous-reader-on-comma)
                     (funcall previous-reader-on-backtick stream char))
                   (simple-reader-error stream "No dispatched quasi-quote reader with name ~S" dispatched-quasi-quote-name)))
             (dispatching-toplevel-quasi-quote-reader (stream char)
               (assert dispatched?)
               (assert (char= start-character char))
               (if (or *dispatched-quasi-quote-name*
                       (char-equal (elt (string dispatched-quasi-quote-name) 0)
                                   (peek-char nil stream t nil t)))
                   (bind ((name (or *dispatched-quasi-quote-name*
                                    (read stream t nil t))))
                     (if (or (eq name dispatched-quasi-quote-name)
                             (string= (string-downcase name) (string-downcase dispatched-quasi-quote-name)))
                         (bind ((*dispatched-quasi-quote-name* nil))
                           (read-quasi-quote t stream))
                         (bind ((*dispatched-quasi-quote-name* name))
                           ;; FIXME this will do something horribly wrong for a `foo()
                           ;; if there's no handler for it and the default reader is called.
                           (read-using-original-reader stream char))))
                   (read-using-original-reader stream char)))
             (read-quasi-quoted-body (stream)
               (if (and body-reader
                        (not dispatched?))
                   (funcall body-reader stream)
                   (if end-character
                       (bind ((*readtable* (copy-readtable)))
                         ;; we must set the syntax on the end char to be like #\)
                         ;; until we read out our entire body. this is needed to
                         ;; make "[... 5] style inputs work where '5' is not
                         ;; separated from ']'.
                         ;; doin this is not necessary when invoked from the nested reader
                         ;; but doesn't hurt either. wastes some cycles, but...
                         (set-syntax-from-char end-character #\) *readtable*)
                         (read-delimited-list end-character stream t))
                       (read stream t nil t))))
             (read-quasi-quote (dispatched? stream)
               (bind ((entering-readtable *readtable*)
                      (entering-quasi-quote-nesting-level *quasi-quote-nesting-level*)
                      (*quasi-quote-depth* (1+ *quasi-quote-depth*))
                      (*quasi-quote-nesting-level* (1+ *quasi-quote-nesting-level*))
                      (*toplevel-readtable* (or *toplevel-readtable* *readtable*))
                      (*readtable* (copy-readtable)))
                 (set-macro-character unquote-character (make-unquote-reader entering-quasi-quote-nesting-level entering-readtable))
                 (bind ((original-sharp-dot-reader (get-dispatch-macro-character #\# #\.)))
                   ;; make sure #. reader is called in the restored readtable-case.
                   ;; TODO this should be generalized to wrap all the readers installed in the readtable? probably it's not worth the trouble...
                   (set-dispatch-macro-character #\# #\. (lambda (&rest args)
                                                           (bind ((*readtable* (copy-readtable)))
                                                             (setf (readtable-case *readtable*) (readtable-case *toplevel-readtable*))
                                                             (apply original-sharp-dot-reader args)))))
                 (when end-character
                   ;; there's no point in handling nesting when we are installed on a pair of parens.
                   ;; e.g. in <element <child>> child should not be qq wrapped... so, we enable a
                   ;; special nested reader when we are installed on a pair of parens.
                   (set-macro-character start-character (make-nested-quasi-quote-reader entering-quasi-quote-nesting-level)))
                 (when readtable-case
                   (setf (readtable-case *readtable*) readtable-case))
                 (bind ((body (read-quasi-quoted-body stream)))
                   (if (functionp quasi-quote-wrapper)
                       (funcall quasi-quote-wrapper body dispatched?)
                       (list quasi-quote-wrapper body)))))
             (make-nested-quasi-quote-reader (entering-quasi-quote-nesting-level)
               (named-lambda nested-quasi-quote-reader (stream char)
                 (declare (ignore char))
                 (assert (>= *quasi-quote-nesting-level* entering-quasi-quote-nesting-level))
                 (bind ((body (read-quasi-quoted-body stream)))
                   (if (functionp nested-quasi-quote-wrapper)
                       (funcall nested-quasi-quote-wrapper body dispatched?)
                       (list nested-quasi-quote-wrapper body)))))
             (make-unquote-reader (entering-quasi-quote-nesting-level entering-readtable)
               (named-lambda unquote-reader (stream char)
                 (declare (ignore char))
                 (bind ((*readtable* (copy-readtable)) ; this is only needed when actually restoring the original readers, but then it's needed inside the recursive READ call down at the end, so wrap it all up with a copy
                        (*quasi-quote-nesting-level* (1- *quasi-quote-nesting-level*))
                        (spliced? (char= (peek-char nil stream t nil t) splice-character)))
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
                     (when end-character
                       (set-macro-character start-character (funcall toplevel-reader-wrapper #'toplevel-quasi-quote-reader))))
                   (bind ((body (read stream t nil t)))
                     (if (functionp unquote-wrapper)
                         (funcall unquote-wrapper body spliced?)
                         (list unquote-wrapper body spliced?)))))))
      (funcall toplevel-reader-wrapper
               (if dispatched?
                   #'dispatching-toplevel-quasi-quote-reader
                   #'toplevel-quasi-quote-reader)))))
