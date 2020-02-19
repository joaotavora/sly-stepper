(defpackage :source-tracking-reader
  (:use :cl)
  (:export
   #:read-tracking-source))
(in-package :source-tracking-reader)

(defclass source-tracking-stream
    (trivial-gray-streams:fundamental-character-input-stream)
  ((current-char :initform 0)
   (understream :initarg :understream :initform (error "required!"))
   (observer :initarg :observer :initform (error "required!"))))

(defmethod trivial-gray-streams:stream-read-char
    ((s source-tracking-stream))
  "Read one character from stream S."
  (with-slots (current-char understream) s
    (prog1
        (read-char understream nil :eof)
      (incf current-char))))

(defmethod trivial-gray-streams:stream-unread-char
    ((s source-tracking-stream) char)
  "Read one character from stream S."
  (with-slots (current-char understream) s
    (prog1
        (unread-char char understream)
      (decf current-char))))

(defmethod trivial-gray-streams:stream-read-char-no-hang
    ((s source-tracking-stream))
  "Read one character from stream S."
  (with-slots (current-char understream) s
    (let ((retval
            (read-char-no-hang  understream nil :eof)))
      (when retval
        (incf current-char)
        retval))))

(defun read-tracking-source (&optional (stream *standard-input*)
                              (eof-error-p t)
                              (eof-value nil)
                              recursive-p
                              (observer (error "must supply observer")))
  "Like CL:READ, but call OBSERVER with read forms and its positions."
  (let ((sts (make-instance 'source-tracking-stream
                            :understream stream
                            :observer observer))
        (original-rt *readtable*))
    (with-slots (observer current-char) sts
      (flet ((read-token-fallback (stream first-char)
               (unread-char first-char stream)
                 (let ((*readtable* original-rt))
                   (read-preserving-whitespace stream nil nil nil)))
             (wrap (char rt fun non-terminating-p)
               (dolist (c (list (char-upcase char)
                                (char-downcase char)))
                 (set-macro-character
                  c
                  (lambda (&rest whatever)
                    (let ((cur current-char)
                          (res
                            (multiple-value-list
                             (apply fun whatever))))
                      (funcall observer (first res)
                               ;; The 1- is because by now a the char
                               ;; has already been read, but not the
                               ;; form.
                               (cons (1- cur) current-char))
                      (values-list res)))
                  non-terminating-p
                  rt))))
        (let ((*readtable*
                (loop with rt = (copy-readtable)
                      for i from 0 upto 96
                      for char = (code-char i)
                      for (fun non-terminating-p)
                        = (multiple-value-list (get-macro-character char))
                      ;; SBCL has nil entries for constituent
                      ;; characters, whitespace or not.  Give the
                      ;; latter a default `READ-TOKEN-FALLBACK' entry
                      ;; which should do the right thing, except if
                      ;; they're whitespace, so check for that here.
                      for macrofun = (or fun
                                         (and (not (member char '(#\Tab
                                                                  #\Newline
                                                                  #\Linefeed
                                                                  #\Page
                                                                  #\Return
                                                                  #\Space)))
                                              #'read-token-fallback))
                      when macrofun
                        do (wrap char rt
                                 macrofun
                                 (if fun non-terminating-p
                                     t))
                      finally
                         (return rt))))
          (read sts eof-error-p eof-value recursive-p))))))


