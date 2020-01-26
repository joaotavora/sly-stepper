(defpackage #:slynk-slepper
  (:use :cl #:slynk-api)
  (:export
   #:slepper))
(in-package #:slynk-slepper)


;; Helpers
(defgeneric maptree (fn tree)
  (:documentation "Map FN across TREE.")
  (:method (fn (cst cst:cst))
    (funcall fn cst)
    (when (cst:consp cst)
      (maptree fn (cst:first cst))
      (maptree fn (cst:rest cst))))
  (:method (fn (tree cons))
    (funcall fn tree)
    (when (consp tree)
      (maptree fn (car tree))
      (maptree fn (cdr tree)))))

(defgeneric report-form (source-tracked-form)
  (:documentation
   "Report SOURCE-TRACKED-FORM to Emacs as a plist.")
  (:method ((cst cst:cst))
    (let ((src (cst:source cst)))
      (assert src nil "~a doesn't have any source-tracking information attached")
      (list :form (cst:raw cst) :source src))))

(defgeneric read-tracking-source (stream)
  (:documentation
   "Like CL:READ, but return a source-tracking information as a second value.
The second value can be passed to RECONSTRUCT or
FORM-SOURCE-LOCATION.")
  (:method (stream)
    (let ((cst (eclector.concrete-syntax-tree:read stream)))
      (values (cst:raw cst)
              cst))))

(defgeneric reconstruct (untracked-expansion
                         source-tracking-info)
  (:documentation
   "Compute tree mirroring UNTRACKED-EXPANSION, but as tracked as possible.
The resulting tree may or may not reuse CONSes and atoms from
UNTRACKED-EXPANSION.  Inside it, as many CONSes and ATOM as possible
shall respond to FORM-SOURCE-LOCATION.")
  (:method ((untracked-expansion cons) (source-tracking-info cst:cst))
    (let ((retval (concrete-syntax-tree:reconstruct
                   untracked-expansion
                   source-tracking-info
                   nil)))
      (values (cst:raw retval)
              retval))))

(defgeneric form-source-location (form source-tracking-info)
  (:documentation
   "Compute source information for FORM from SOURCE-TRACKING-INFO.")
  (:method (form (cst cst:cst))
     (maptree (lambda (cst)
                (when (eq (cst:raw cst) form)
                  (return-from form-source-location
                    (cst:source cst))))
              cst)))

(defun collect-interesting-forms (reconstructed source-tracking-info)
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
             (mapc #'explore (butdeclares (butdoc body)))))
         (explore-body (forms)
           (mapc #'explore (butdeclares forms)))
         (explore (form)
           (format *trace-output* "Exploring ~a~%" form)
           (when (form-source-location form source-tracking-info)
             (push form interesting))
           (when (consp form)
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
                (loop for (nil val) on things by #'cddr do (explore val)))
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
               #+sbcl
               ((sb-int:named-lambda &rest definition)
                (explore-definition definition))
               #+ccl
               ((ccl:nfunction &rest definition)
                (explore-definition (cdr definition)))
               #+allegro
               ((defun &rest definition)
                (explore-definition definition))
               (t
                (let ((op (first form)))
                  (assert (symbolp op) nil "Suprised by ~a" form)
                  (when (fboundp op)
                    (mapc #'explore (rest form)))))))))
      (explore reconstructed))))

;; Algorithm

(defslyfun slepper (&optional (string "(loop for x from 1 repeat 10 collect x)"))
  "Provide slepperish functionality for the Emacs side of SLY"
  (with-input-from-string (stream string)
    (multiple-value-bind (form-tree source-tracking-info)
        (read-tracking-source stream)
      (let* (#-allegro
             (expanded (trivial-macroexpand-all:macroexpand-all form-tree))
             #+allegro
             (expanded (macroexpand-dammit:macroexpand-dammit form-tree))
             )
        (multiple-value-bind (reconstructed source-tracking-info)
            (reconstruct expanded source-tracking-info)
          (let ((interesting
                  (collect-interesting-forms reconstructed
                                             source-tracking-info)))
            (values
             (mapcar #'report-form interesting)
             interesting
             reconstructed)))))))

(provide 'slynk-slepper)
