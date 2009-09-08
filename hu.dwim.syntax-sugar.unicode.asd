;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.syntax-sugar.unicode
  :class hu.dwim.system
  :author ("Levente Mészáros <levente.meszaros@gmail.com>"
           "Attila Lendvai <attila.lendvai@gmail.com>"
           "Tamás Borbély <tomi.borbely@gmail.com>")
  :licence "BSD / Public domain"
  :description "Various unicode syntax extensions"
  :depends-on (:hu.dwim.syntax-sugar)
  :components ((:module "source"
                :components ((:module "unicode"
                              :components ((:file "package")
                                           (:file "one-liners" :depends-on ("package"))))))))
