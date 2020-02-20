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


;; problem! the atom HMM gets incorrectly stickered
(lambda ()
  (let ((hmm (progn
               (if thingy
                   foo
                   bar))))
    hmm
    aha))

(format t
        (restart-case
            (error "bla")
          (foo ()
            :test shiiz)))

(lambda () (if (foo) (bar) (shod))) ;; OK

(lambda () (if (foo) (bar) (quux (quuz (quuf))))) ;; OK


