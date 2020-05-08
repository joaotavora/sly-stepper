;;; sly-stepper.el --- A template SLY contrib  -*- lexical-binding: t; -*-
;;
;; Version: 0.1
;; URL: https://github.com/capitaomorte/sly-stepper
;; Keywords: languages, lisp, sly
;; Package-Requires: ((sly "1.0.0-beta2"))
;; Author: João Távora <joaotavora@gmail.com>
;;
;; Copyright (C) 2015 João Távora
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; `sly-stepper` is SLY contrib. See README.md
;;
;;; Code:

;; Uncomment this and set to your paths to appease Flymake's elisp
;; backend:
;;
;; (eval-when-compile
;;   (add-to-list 'load-path "~/Source/Emacs/sly")
;;   (add-to-list 'load-path "~/Source/Emacs/sly/contrib"))
(require 'cl-lib)
(require 'sly)
(require 'sly-stickers)

(define-sly-contrib sly-stepper
  "Define the `sly-stepper' contrib.
Depends on the `slynk-stepper' ASDF system Insinuates itself
in `sly-editing-mode-hook', i.e. lisp files."
  (:slynk-dependencies slynk-stepper)
  (:on-load (add-hook 'sly-editing-mode-hook 'sly-stepper-mode))
  (:on-unload (remove-hook 'sly-editing-mode-hook 'sly-stepper-mode)))

(defun sly-stepper--stepper-sticker-p (sticker)
  (overlay-get sticker 'sly-stepper--stepper-sticker-p))

(defun sly-stepper--stepper-sticker (from to function-name)
  (let ((sticker (sly-stickers--sticker from to)))
    (overlay-put sticker 'sly-stepper--stepper-sticker-p t)
    (overlay-put sticker 'sly-stepper--function-name function-name)))

(defun sly-stepper (pos)
  "Instrument nearest function at POS for stepping.
POS defaults to current point."
  (interactive "d")
  (cl-destructuring-bind (beg end)
      (sly-region-for-defun-at-point pos)
    (save-excursion
      (let* ((all-stickers (sly-stickers--stickers-between beg end)))
        (when (and (cl-remove-if #'sly-stepper--stepper-sticker-p all-stickers)
                   (not
                    (y-or-n-p
                   "[sly] Some non-stepper stickers in region.  Delete them?")))
          (user-error "[sly] Aborted."))
        (mapc #'sly-stickers--delete all-stickers))
      (save-restriction
        (narrow-to-region beg end)
        (goto-char (point-min))
        (cl-loop
         with function-name =
         (and (search-forward-regexp "^[\s\t]*(def[^\s\t]*[\s\t]+"
                                     (line-end-position) t)
              (substring-no-properties
               (thing-at-point 'symbol)))
         for result in (sly-eval
                        `(slynk-stepper:stepper
                          :string
                          ,(buffer-substring-no-properties beg end)))
         for (a . b) = (cl-getf result :source)
         for from = (+ beg a) for to = (+ beg b)
         unless (zerop a)
         do (sly-stepper--stepper-sticker from to function-name)
         and minimize from into min
         and maximize to into max
         finally
         (let ((top-level-sticker
                (and min max
                     (car
                      (sly-stickers--stickers-exactly-at min max)))))
           (if (null top-level-sticker)
               (sly-message "Something odd instrumented for stepping")
             (sly-message "%s now instrumented for stepping."
                          (if function-name (upcase function-name)
                            "Unknown top-level form"))
             (overlay-put top-level-sticker 'sly-stickers--top-level t))))))))

(defvar sly-stepper-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-s P") 'sly-stepper)
    map)
  "A keymap accompanying `sly-stepper-mode'.")

(define-minor-mode sly-stepper-mode
  "A minor mode for using the SLY/Emacs stepper."
  nil nil nil)


;;; Automatically add ourselves to `sly-contribs' when this file is loaded
;;;###autoload
(with-eval-after-load 'sly
  (add-to-list 'sly-contribs 'sly-stepper 'append))

(provide 'sly-stepper)
;;; sly-stepper.el ends here
