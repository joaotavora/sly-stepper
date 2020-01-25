(defpackage #:slynk-slepper
  (:use :cl #:slynk-api)
  (:export
   #:slepper))
(in-package #:slynk-slepper)

(defun maptree (fn tree &key
                 (consp #'consp)
                 (car #'car)
                 (cdr #'cdr))
  (funcall fn tree)
  (when (funcall consp tree)
    (maptree fn (funcall car tree)
             :consp consp :car car :cdr cdr)
    (maptree fn (funcall cdr tree)
             :consp consp :car car :cdr cdr)))

(defun expanded-p (cst erst)
  "Tell if CST raw node is in expanded raw syntax tree ERST."
  (or (eq (cst:raw cst) erst)
      (and (consp erst)
           (or (expanded-p cst (car erst))
               (expanded-p cst (cdr erst))))))

(defslyfun slepper (&optional (string "(loop for x from 1 repeat 10 collect x)"))
  "Provide slepperish functionality for the Emacs side of SLY"
  (with-input-from-string (stream string)
    (let* ((tree (eclector.concrete-syntax-tree:read stream))
           (expanded (trivial-macroexpand-all:macroexpand-all (cst:raw tree)))
           recovered)
      (maptree (lambda (cst)
                 (when (and (expanded-p cst expanded)
                            (not (cst:null cst)))
                   (pushnew cst recovered)))
               tree
               :consp #'cst:consp :car #'cst:first :cdr #'cst:rest)
      (mapcar (lambda (cst)
                (list :form (cst:raw cst) :source (cst:source cst)))
              recovered))))


(provide 'slynk-slepper)
