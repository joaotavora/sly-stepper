;;; -*- lisp -*-
(in-package :asdf)

(defsystem :slynk-slepper
    :author "João Távora <https://github.com/capitaomorte>"
  :depends-on (#:slynk #:agnostic-lizard)
  :description "Slynk part of the SLY/Emacs stepper tool."
  :components ((:file "source-tracking-reader")
               (:file "slynk-slepper")))

;; Local Variables:
;; coding: utf-8
;; End:
