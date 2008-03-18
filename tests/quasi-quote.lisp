;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-syntax-sugar-test)

(defsuite* (test/quasi-quote :in test))

(deftest test/quasi-quote/simple ()
  (enable-readtime-wrapper-syntax)
  (bind ((expected '(my-quote
                     (1 2 (my-unquote 3 nil) (my-unquote 4 t) 5))))
    (is (equal expected
               (read-from-string
                "{(with-quasi-quote-syntax my-quote my-unquote)
                `(1 2 ,3 ,@4 5)}")))
    (is (equal expected
               (read-from-string
                "{(with-quasi-quote-syntax my-quote my-unquote :quasi-quote-character #\\$
                                                               :unquote-character #\\;)
                $(1 2 ;3 ;@4 5)}")))
    (is (equal expected
               (read-from-string
                "{(with-quasi-quote-syntax my-quote my-unquote :quasi-quote-character #\\[
                                                               :quasi-quote-end-character #\\]
                                                               :unquote-character #\\;
                                                               :splice-character #\\!)
                  [1 2 ;3 ;!4 5]}")))
    ;; this is a bit crazy: quasi-quote is registered on the same {} chars as the wrapping
    ;; readtime-wrapper, but quasi-quote restores the syntax when its scope is left, so
    ;; everything's fine with this test.
    (is (equal expected
               (read-from-string
                "{(with-quasi-quote-syntax my-quote my-unquote :quasi-quote-character #\\{
                                                               :quasi-quote-end-character #\\}
                                                               :unquote-character #\\;
                                                               :splice-character #\\!)
                  {1 2 ;3 ;!4 5}}")))))

(deftest test/quasi-quote/end-character-reader-restoration ()
  (enable-readtime-wrapper-syntax)
  (bind ((42-reader (lambda (stream char)
                      (declare (ignore stream char))
                      42)))
    (set-macro-character #\[ 42-reader)
    (set-macro-character #\] 42-reader))
  (bind ((input "[{(with-quasi-quote-syntax my-quote my-unquote :quasi-quote-character #\\[
                                                                :quasi-quote-end-character #\\]
                                                                :unquote-character #\\;
                                                                :splice-character #\\!)
                   [1 2 ;3 ;!4 5]]}]")
         ((:values value position)))
    (setf (values value position) (read-from-string input))
    (is (= value 42))
    (setf (values value position) (read-from-string input t nil :start position))
    (is (equal value
               '(progn
                 (my-quote (1 2 (my-unquote 3 nil) (my-unquote 4 t) 5))
                 42)))))

(deftest test/quasi-quote/nested-lisp-quasi-quote ()
  (enable-readtime-wrapper-syntax)
  (is (equal '(my-quote
               (1 (my-unquote `(foo ,(+ 40 2)) nil) 3))
             (read-from-string
              "{(with-quasi-quote-syntax my-quote my-unquote)
                `(1 ,`(foo ,(+ 40 2)) 3)}")))
  (bind ((expected '(my-quote
                     (1
                      (my-quote
                       (bar (my-unquote
                             (my-unquote
                              `(foo ,(+ 40 2))
                              nil)
                             nil)))
                      3))))
    (is (equal expected
               (read-from-string
                "{(with-quasi-quote-syntax my-quote my-unquote :quasi-quote-character #\\$ :unquote-character #\\;)
                  $(1 $(bar ;;`(foo ,(+ 40 2))) 3)}")))
    (is (equal expected
               (read-from-string
                "{(with-quasi-quote-syntax my-quote my-unquote)
                  `(1 `(bar ,,`(foo ,(+ 40 2))) 3)}")))
    (is (equal expected
               (read-from-string
                "{(with-quasi-quote-syntax my-quote my-unquote :quasi-quote-character #\\[
                                                               :quasi-quote-end-character #\\]
                                                               :unquote-character #\\;)
                  [1 [bar ;;`(foo ,(+ 40 2))] 3]}")))))
