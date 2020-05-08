;;; -*- lisp -*-
(in-package :asdf)

(defsystem :slynk-stepper
    :author "João Távora <https://github.com/capitaomorte>"
  :depends-on (#:slynk
               ;; This is a "soft" dependency,
               ;; #:agnostic-lizard
               )
  :description "Slynk part of the SLY/Emacs stepper tool."
  :components ((:file "source-tracking-reader")
               (:file "slynk-stepper")))

;; Local Variables:
;; coding: utf-8
;; End:
