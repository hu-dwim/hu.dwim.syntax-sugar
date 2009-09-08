;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.syntax-sugar
  :class hu.dwim.system
  :author ("Levente Mészáros <levente.meszaros@gmail.com>"
           "Attila Lendvai <attila.lendvai@gmail.com>"
           "Tamás Borbély <tomi.borbely@gmail.com>")
  :licence "BSD / Public domain"
  :description "Various syntax extensions"
  :depends-on (:hu.dwim.common-lisp)
  :components ((:module "source"
                :components ((:file "package")
                             (:file "duplicates" :depends-on ("package"))
                             (:file "syntax-sugar" :depends-on ("duplicates"))
                             (:file "one-liners" :depends-on ("syntax-sugar"))
                             (:file "readtime-wrapper" :depends-on ("one-liners"))
                             (:file "quasi-quote" :depends-on ("one-liners"))
                             (:file "feature-cond" :depends-on ("one-liners"))
                             (:file "string-quote" :depends-on ("one-liners"))))))
