;;; -*- lisp -*-
(in-package :asdf)

(defsystem :slynk-slepper
    :author "João Távora <https://github.com/capitaomorte>"
  :depends-on (#:slynk
               #:eclector-concrete-syntax-tree
               #:trivial-macroexpand-all)
  :description "SLEPPER support for Slynk"
  :components ((:file "slepper")
               (:file "slynk-slepper")))

;; Local Variables:
;; coding: utf-8
;; End:
