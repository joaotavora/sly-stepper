(defpackage #:slynk-slepper
  (:use :cl #:slynk-api)
  (:export
   #:slepper))

(in-package #:slynk-slepper)

(defun mnesic-macroexpand-all (form ht-1 rht-1)
  (let (stack
        (expansion-positions (make-hash-table))
        all-ever-expanded)
    (values
     (agnostic-lizard:walk-form
      form nil
      :on-every-form-pre
      (lambda (subform env)
        (declare (ignore env))
        (push subform all-ever-expanded)
        (format *trace-output* "~&~aGoing IN  ~a~%"
                (make-string (length stack) :initial-element #\Space)
                subform)
        (push
         (list
          :original subform
          :at (gethash subform ht-1))
         stack)
        subform)
      :on-every-form
      (lambda (expansion env)
        (declare (ignore env))
        (format *trace-output* "~&~aGoing OUT  ~a~%"
                (make-string (1- (length stack)) :initial-element #\Space)
                expansion)
        (destructuring-bind (&key original ((:at locations)))
            (pop stack)
          (setf
           (gethash expansion expansion-positions)
           (list :original original
                 :at locations)))
        expansion))
     expansion-positions)))

(defvar *compound-form-location*)

(defun forms-of-interest (expanded ht-2)
  (let (interesting)
    (labels
        ((butdoc (forms)
           (member-if-not #'stringp forms))
         (butdeclares (forms)
           (member-if-not (lambda (form)
                            (and (consp form)
                                 (eq 'declare (first form))))
                          forms))
         (explore-definition (definition)
           (format *trace-output* "Exploring definition ~a~%" definition)
           (destructuring-bind (name arglist &rest body)
               definition
             (declare (ignore name arglist))
             (explore-body body)))
         (explore-body (forms)
           (mapc #'explore (butdeclares (butdoc forms))))
         (maybe-explore-atom (form)
           (when (and (atom form)
                      form
                      (not (stringp form))
                      (not (keywordp form)))
             (loop with entry = (gethash form ht-2)
                   with original = (getf entry :original)
                   for loc in (getf entry :at)
                   when (and *compound-form-location*
                             (containsp *compound-form-location* loc))
                     do (push (list :form form
                                    :original original
                                    :source loc)
                              interesting))))
         (explore (form)
           "Called when FORM is deemed interesting."
           (format *trace-output* "Exploring ~a~%" form)
           (when (consp form)
             (let* ((entry (gethash form ht-2))
                    (loc (first (getf entry :at))))
               (when loc
                 (push (list :form form
                             :original (getf entry :original)
                             :source loc)
                       interesting))
               (slynk-api:destructure-case
                   form
                 ((block name &rest body)
                  (declare (ignore name))
                  (mapc #'explore (butdeclares body)))
                 ((return-from name &optional value)
                  (declare (ignore name))
                  (explore value))
                 ((catch tag &rest body)
                  (explore tag)
                  (mapc #'explore body))
                 ((load-time-value form &optional read-only-p)
                  (declare (ignore form read-only-p)))
                 ((setq &rest things)
                  (loop for (nil val) on things by #'cddr
                        do (explore val)))
                 ((eval-when syms &rest body)
                  (when (member :execute syms)
                    (explore-body body)))
                 ((locally &rest body)
                  (explore-body body))
                 ((symbol-macrolet macrobindings &rest body)
                  (declare (ignore macrobindings))
                  (explore-body body))
                 ((flet definitions &rest body)
                  (mapc #'explore-definition definitions)
                  (explore-body body))
                 ((macrolet definitions &rest body)
                  (declare (ignore definitions))
                  (explore-body body))
                 ((tagbody &rest statements)
                  (mapc #'explore (remove-if #'atom statements)))
                 ((function thing)
                  (explore thing))
                 ((multiple-value-call function &rest arguments)
                  (explore function)
                  (mapc #'explore arguments))
                 ((the value-type form)
                  (declare (ignore value-type))  (explore form))
                 ((go tag) (declare (ignore tag)))
                 ((multiple-value-prog1 values-form &rest body)
                  (explore values-form)
                  (explore-body body))
                 ((throw tag result)
                  (explore tag) (explore result))
                 ((if test then &optional else)
                  (explore test) (explore then) (when else (explore else)))
                 ((progn &rest forms)
                  (mapc #'explore forms))
                 ((unwind-protect protected &rest cleanup)
                  (explore protected) (mapc #'explore cleanup))
                 ((labels definitions &rest body)
                  (explore-body body)
                  (mapc #'explore-definition definitions))
                 ((progv vars vals &rest body)
                  (explore vars)
                  (explore vals)
                  (explore-body body))
                 ((let* bindings &rest body)
                  (mapc #'explore
                        (mapcar #'second
                                (remove-if-not #'consp bindings)))
                  (explore-body body))
                 ((let bindings &rest body)
                  (mapc #'explore
                        (mapcar #'second
                                (remove-if-not #'consp bindings)))
                  (explore-body body))
                 ((quote thing)
                  (declare (ignore thing)))
               ;;; Quirks section
                 ;;
                 ;; * even though LABMDA is a macro, it expands to
                 ;; (function (lambda ..)) i.e. to itself, so we must
                 ;; handle it explicitly.
                 ((lambda arglist &rest body)
                  (declare (ignore arglist))
                  (explore-body body))
                 ;; * AGNOSTIC-LIZARD will refuse the expand the
                 ;; following by default (probably for good reason) so
                 ;; just add them here.
                 ((defun name arglist &rest body)
                  (declare (ignore name arglist))
                  (explore-body body))
                 ((defmethod name arglist &rest body)
                  (declare (ignore name arglist))
                  (explore-body body))
                 ((defmacro name arglist &rest body)
                  (declare (ignore name arglist))
                  (explore-body body))
                 ((cond &rest clauses)
                  (mapc #'explore (mapcar #'first clauses))
                  (mapc #'explore (mapcar #'second clauses)))
                 ((multiple-value-bind spec val &rest body)
                  (declare (ignore spec))
                  (explore val)
                  (explore-body body))
                 ((handler-bind bindings &rest body)
                  (mapc #'explore (mapcar #'second bindings))
                  (explore-body body))
                 (t
                  (let ((op (first form)))
                    (assert (symbolp op) nil "Suprised by ~a" form)
                    (let ((*compound-form-location* loc))
                      (mapc #'maybe-explore-atom (rest form)))
                    (mapc #'explore (rest form)))))))))
      (explore expanded)
      interesting)))

;; Algorithm

(defun containsp (a b)
  "True iff A contains B."
  (and (< (car a) (car b))
       (> (cdr a) (cdr b))))

(defslyfun slepper (&optional (string "(loop for x from 1 repeat 10 collect x)"))
  "Provide slepperish functionality for the Emacs side of SLY"
  (with-input-from-string (stream string)
    (let* ((ht-1 (make-hash-table))
           (rht-1 (make-hash-table :test #'equal))
           (form-tree
             (source-tracking-reader:read-tracking-source stream nil
              nil nil
              (lambda (form start end)
                (let ((this-location (cons start end)))
                  (setf (gethash this-location rht-1)
                        (list :theform form
                              :parents nil))
                  (when (consp form)
                    (loop for subform in form
                          do (loop for location
                                     in (gethash subform ht-1)
                                   when (containsp this-location location)
                                     do
                                        (push form
                                              (getf (gethash
                                                     location rht-1) :parents)))))
                  (push this-location (gethash form ht-1)))))))
      (terpri)(terpri)
      (maphash (lambda (k v)
                 (format t "~&~s => ~s~%" k v))
               rht-1)
      ;; (terpri)(terpri)
      ;; (maphash (lambda (k v)
      ;;            (format t "~&~s => ~s~%" k v))
      ;;          ht-1)
      ;; (terpri)(terpri)
      (multiple-value-bind (expanded ht-2)
          (mnesic-macroexpand-all form-tree ht-1 rht-1)
        ;; (maphash (lambda (k v)
        ;;          (format t "~&~s => ~s~%" k v))
        ;;          ht-2)
        ;; (terpri)(terpri)
        (forms-of-interest expanded ht-2)))))

(provide 'slynk-slepper)
