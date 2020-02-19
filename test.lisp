(defpackage :slepper-tests (:use :cl))
(in-package :slepper-tests)


(defun bla ()
  (loop repeat 3
        do (+ (* 2 3)
              (* 4 5))))

(defun blo ()
  (function-call
   (if foo
       bar
       baz)))


(defun teste-restarts ()
  (format t
   (restart-case
       (error "bla")
     (foo ()
       :test (lambda (c)
               (format t "~&Checking restart FOO for condition ~a~%" c)
               (force-output)
               t)))))

