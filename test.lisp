(defpackage :slepper-tests (:use :cl))
(in-package :slepper-tests)


(in-package :slynk-slepper)


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

(defun coiso () (cond ((foo) (bar)) (t (shod))))

(defmacro tricky-maybe (form) `(when bla ,(copy-list form)))

(defmacro maybe (form) `(when (zerop (random 2)) ,form))

(defmacro tricky-maybe (form) `(when (zerop (random 2)) ,(copy-list form)))

(lambda (x) (if (maybe (minusp x)) (- x) x))

(lambda (x) (if (tricky-maybe (minusp x)) (- x) x))


(let ((foo (bar))) (quux))

(let ((foo (bar))) (quux) foo)

(let ((bla b) (+ b b)))

(return (foo))

(delete 'cond agnostic-lizard::*hardwired-operators*)

(lambda () (cond ((foo) (bar)) ((baz) (quux)) (t 42)))

(lambda () (frobnicate foo bar))

(loop repeat (foo) do (bar))

(lambda () (cons #1=(foo 42) . (#1#)))

(lambda () (let ((x (foo))) x))

(lambda (x) x)

(slepper
 "")

(defun bla ()
  (+ 42 42))

(lambda () (loop for x in (foo) collect x))


;; => ((:FORM (- X) :SOURCE (24 . 29)) (:FORM (MINUSP X) :SOURCE (12 . 22)))
(slepper
 "(if (tricky (minusp x)) (- x) x)")
;; => ((:FORM (- X) :SOURCE (24 . 29)))

(defun my-abs (x)
  (if (tricky (minusp x)) (- x) x))










