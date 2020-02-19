;;; -*- lisp -*-
(in-package :asdf)

(defsystem :slynk-slepper
    :author "João Távora <https://github.com/capitaomorte>"
  :depends-on (#:slynk
               #:eclector-concrete-syntax-tree
               #:trivial-gray-streams
               #-allegro
               #:trivial-macroexpand-all
               #+allegro
               #:macroexpand-dammit)
  :description "SLEPPER support for Slynk"
  :components ((:file "source-tracking-reader")
               (:file "slynk-slepper")))

;; Local Variables:
;; coding: utf-8
;; End:
