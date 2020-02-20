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
  (:method (fn tree)
    (funcall fn tree)
    (when (consp tree)
      (maptree fn (car tree))
      (maptree fn (cdr tree)))))

(defparameter *use-eclector* nil)

(defun read-tracking-source (stream)
  "Like CL:READ, but return a source-tracking information as a second value.
The second value can be passed to RECONSTRUCT or
FORM-SOURCE-LOCATION."
  (if *use-eclector*
      (let ((cst (eclector.concrete-syntax-tree:read stream)))
        (values (cst:raw cst)
                cst))
      (let ((ht (make-hash-table)))
        (values
         (source-tracking-reader:read-tracking-source
          stream nil nil nil
          (lambda (form pos)
            (setf (gethash form ht) pos)))
         ht))))

(defgeneric reconstruct (untracked-expansion
                         source-tracking-info)
  (:documentation
   "Compute tree mirroring UNTRACKED-EXPANSION, but as tracked as possible.

Return two values.  The resulting CONS tree is returned as the first
value and may or may not reuse CONSes and atoms from
UNTRACKED-EXPANSION.  As a second value return updated source-tracking
information.

Inside the resulting, as many CONSes and ATOM as possible shall
respond to FORM-SOURCE-LOCATION when that function is also passed the
updated source-tracking information.")
  (:method ((untracked-expansion cons) (source-tracking-info cst:cst))
    (let ((retval (concrete-syntax-tree:reconstruct
                   untracked-expansion
                   source-tracking-info
                   nil)))
      (values (cst:raw retval)
              retval)))
  (:method ((untracked-expansion cons) (source-tracking-info hash-table))
    ;; naive
    (values untracked-expansion source-tracking-info)
    (let ((new-ht (make-hash-table)))
      (labels ((traverse (form)
                 (let* ((mirror
                          (cond ((consp form)
                                 (cons
                                  (traverse (car form))
                                  (traverse (cdr form))))
                                (t
                                 form)))
                        (source (gethash form source-tracking-info)))
                   (when source
                     (setf (gethash mirror new-ht) source))
                   mirror)))
        (values (traverse untracked-expansion)
                new-ht
                untracked-expansion
                source-tracking-info)))))

(defgeneric form-source-location (form source-tracking-info)
  (:documentation
   "Compute source information for FORM from SOURCE-TRACKING-INFO.")
  (:method (form (cst cst:cst))
     (maptree (lambda (cst)
                (when (eq (cst:raw cst) form)
                  (return-from form-source-location
                    (cst:source cst))))
              cst))
  (:method (form (source-tracking-info hash-table))
    (gethash form source-tracking-info)))

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
             (explore-body body)))
         (explore-body (forms)
           (mapc #'explore (butdeclares (butdoc forms))))
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
               ;;; Quirks section
               ;;
               ;; * lambda is a macro that expands to (function (lambda
               ;; ...)) but TRIVIAL-MACROEXPAND-ALL:MACROEXPAND-ALL
               ;; doesn't see it like that and keeps it unexpanded.
               ((lambda arglist &rest body)
                (declare (ignore arglist))
                (explore-body body))
               ;; * DEFUN will often expand to implementation-defined
               ;;   macros that can't be expanded by macroexpand.
               ;;
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
                  (mapc #'explore (rest form))))))))
      (explore reconstructed)
      interesting)))

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
             (mapcar (lambda (form)
                       (let ((src (form-source-location form source-tracking-info)))
                         (assert src nil "~a doesn't have any source-tracking information attached")
                         (list :form form :source src)))
                     interesting)
             interesting
             reconstructed)))))))

(provide 'slynk-slepper)
