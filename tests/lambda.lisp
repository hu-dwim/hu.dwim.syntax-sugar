;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-syntax-sugar-test.sharp-l)

(eval-when (:compile-toplevel)
  (setup-readtable))

(defsuite* (test/lambda :in test))

(deftest perform-sharp-l-test (fn args expected)
  (bind ((result (apply fn args)))
    (if (functionp expected)
        (funcall expected result)
        (is (equal expected result)))))

(defmacro define-sharp-l-test (name &body body)
  `(deftest ,name ()
     ,@(mapcar (lambda (entry)
                 (if (member (first entry) '(signals is))
                     entry
                     (bind (((fn args expected) entry))
                       `(perform-sharp-l-test ,fn ',args ,expected))))
               body)))

(define-sharp-l-test test/lambda/simple
  (#L42 () 42)
  (#L(+ !1 !2) (2 40) 42))

(define-sharp-l-test test/lambda/block-label-is-ignored
  (#L(block !2 (return-from !2 !1)) (6) 6))

(define-sharp-l-test test/lambda/no-variable-in-quote
  (#L'!1 () '!1))

(define-sharp-l-test test/lambda/not-captures-outer-bang
  ((let ((!1 42))
      (declare (ignore !1))
      #L!1)
   (69)
   69))

(define-sharp-l-test test/lambda/nested
  (#L#L1 () (lambda (fn)
              (is (= (funcall fn) 1))))
  (#L#L!1 () (lambda (fn)
               (is (= (funcall fn 42) 42))))
  (#L#L(+ !1 !2) () (lambda (fn)
                      (is (= (funcall fn 40 2) 42)))))

(deftest test/lambda/complex-nested ()
  (is (string=
       "BARfoo"
       (funcall (funcall #L(let ((a !1))
                             #L(concatenate 'string (string-upcase !1) a))
                         "foo")
                "bar"))))

#||
;; TODO ?! it's too late...

(test sharpl-symbol-macrolet-1
  (is (eql 3 (symbol-macrolet ((sym !1)) (funcall #Lsym 3)))))

(test sharpl-symbol-macrolet-2
  (is (eql 3 (funcall (symbol-macrolet ((sym !1)) #Lsym) 3))))

(define-sharp-l-test test/lambda/symbol-macrolet
  ("(symbol-macrolet ((sym !1)) #Lsym)" (3) 3)
  ("(symbol-macrolet ((sym !1)) (funcall #Lsym 42))" (3) 3))
||#

(define-sharp-l-test test/lambda/symbol-macrolet/inner
  (#L(symbol-macrolet ((!2 !1)) (+ !2 10))
   (5)
   15))

(deftest test/lambda/macrolet/1 ()
  (is (equal
       15
       (macrolet ((mac (arg) `(+ !1 ,arg)))
         (funcall #L(mac 10) 5)))))

(deftest test/lambda/macrolet/2 ()
  (is (equal
       15
       (funcall (macrolet ((mac (arg) `(+ !1 ,arg)))
                  #L(mac 10))
                5))))

(define-sharp-l-test test/lambda/macrolet/inner
  (#L(macrolet ((!2 () '!1)) (!2))
   (15)
   15))

(deftest test/lambda/bang-binds-to-innermost ()
  (is (equal
       10
       (funcall (funcall #L(let ((a !1))
                             #L(+ a !1))
                         6)
                4))))

(deftest test/lambda/interposed-macrolet ()
  (is (equal
       6
       (funcall (funcall #L(macrolet ((mac () '!1))
                             #L(mac)))
                6))))

(deftest test/lambda/nested-macrolet ()
  (is (equal
       21
       (funcall (funcall #L(macrolet ((return-bang () ''!1))
                             (macrolet ((multiply-first-bang (arg) `(* ,arg ,(return-bang))))
                               #L(+ (multiply-first-bang 2) 1))))
                10))))

(deftest test/lambda/interposed-symbol-macrolet ()
  (is (equal
       10
       (funcall (funcall #L(symbol-macrolet ((mac !1))
                             #Lmac))
                10))))
