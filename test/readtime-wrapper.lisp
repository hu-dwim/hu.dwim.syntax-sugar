;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.syntax-sugar.test)

(defsuite* (test/readtime-wrapper :in test))

(def definer readtime-wrapper-test (name args &body body)
  `(deftest ,name ,args
     ,@(mapcar (lambda (entry)
                 (if (member (first entry) '(signals is))
                     entry
                     (bind (((comparator expected string) entry))
                       `(is (,comparator ,expected
                                         (read-from-string/with-readtime-wrapper-syntax ,string))))))
               body)))

(def function read-from-string/with-readtime-wrapper-syntax (string)
  (with-local-readtable
    (enable-readtime-wrapper-syntax)
    (read-from-string string)))

(def test test/readtime-wrapper/read-suppress ()
  (is (null (bind ((*read-suppress* t))
              (finishes (read-from-string/with-readtime-wrapper-syntax
                         "{(with-package :hu.dwim.syntax-sugar) foo}"))))))

(def readtime-wrapper-test test/readtime-wrapper/with-package ()
  (eq 'hu.dwim.syntax-sugar::foo
      "{(with-package :hu.dwim.syntax-sugar)
        foo}")
  (equal '(progn
           hu.dwim.syntax-sugar::foo
           hu.dwim.syntax-sugar::bar)
         "{(with-package :hu.dwim.syntax-sugar)
           foo
           bar}")
  (string= "foo"
           "{(with-package :hu.dwim.syntax-sugar)
             \"foo\"}"))

(def readtime-wrapper-test test/readtime-wrapper/sharp-boolean ()
  (signals reader-error (read-from-string "#t"))
  (eq t
      "{with-sharp-boolean-syntax
        #t}")
  (eq nil
      "{with-sharp-boolean-syntax
        #f}")
  (signals reader-error (read-from-string "#f")))
