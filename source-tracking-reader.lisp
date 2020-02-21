(defpackage :source-tracking-reader
  (:use :cl)
  (:export
   #:read-tracking-source))
(in-package :source-tracking-reader)

;; character counting stream
(defclass char-counting-stream
    (trivial-gray-streams:fundamental-character-input-stream)
  ((char-count :initform 0 :reader char-count)
   (understream :initarg :understream :initform (error "required!"))))

(defmethod trivial-gray-streams:stream-read-char
    ((s char-counting-stream))
  "Read one character from stream S."
  (with-slots (char-count understream) s
    (prog1 (read-char understream nil :eof)
      (incf char-count))))

(defmethod trivial-gray-streams:stream-unread-char
    ((s char-counting-stream) char)
  "Read one character from stream S."
  (with-slots (char-count understream) s
    (prog1 (unread-char char understream)
      (decf char-count))))

(defmethod trivial-gray-streams:stream-read-char-no-hang
    ((s char-counting-stream))
  "Read one character from stream S."
  (with-slots (char-count understream) s
    (let ((retval (read-char-no-hang  understream nil :eof)))
      (when retval
        (incf char-count)
        retval))))

(defun char-counting-stream (understream)
  (make-instance 'char-counting-stream :understream understream))

;; substitution-table
(defun substitution-table (original-rt wrapper)
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
                (funcall wrapper
                         (lambda () (apply fun whatever))))
              non-terminating-p
              rt)))
         (standard-whitespace-p (char)
           (member char '(#\Tab      #\Newline
                          #\Linefeed #\Page
                          #\Return   #\Space))))
    (loop with rt = (copy-readtable original-rt)
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
                             (and (not (standard-whitespace-p char))
                                  #'read-token-fallback))
          when macrofun
            do (wrap char rt
                     macrofun
                     (if fun non-terminating-p t))
          finally
             (return rt))))

;; entry point
(defun read-tracking-source
    (&optional (stream *standard-input*)
       (eof-error-p t) eof-value
       recursive-p (observer (lambda (&rest ignore)
                               (declare (ignore ignore)))))
  (let* ((ccs (char-counting-stream stream))
         (*readtable*
           (substitution-table
            *readtable*
            (lambda (shadowed-entry)
              (let ((start (1- (char-count ccs)))
                    (result (funcall shadowed-entry))
                    (end (char-count ccs)))
                (prog1 result
                  (funcall observer result start end)))))))
    (read ccs eof-error-p eof-value recursive-p)))
