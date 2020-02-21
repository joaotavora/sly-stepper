;;; -*- lisp -*-
(in-package :asdf)

(defsystem :slynk-slepper
    :author "João Távora <https://github.com/capitaomorte>"
  :depends-on (#:slynk
               #:trivial-gray-streams
               #:agnostic-lizard)
  :description "SLEPPER support for Slynk"
  :components ((:file "source-tracking-reader")
               (:file "slynk-slepper")))

;; Local Variables:
;; coding: utf-8
;; End:
