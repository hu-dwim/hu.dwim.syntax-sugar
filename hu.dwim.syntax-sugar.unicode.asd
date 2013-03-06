;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(defsystem :hu.dwim.syntax-sugar.unicode
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.system"
  :description "Various unicode syntax extensions."
  :depends-on (:hu.dwim.syntax-sugar)
  :components ((:module "source"
                :components ((:module "unicode"
                              :components ((:file "package")
                                           (:file "one-liners" :depends-on ("package"))))))))
