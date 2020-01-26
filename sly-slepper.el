;;; sly-slepper.el --- A template SLY contrib  -*- lexical-binding: t; -*-
;;
;; Version: 0.1
;; URL: https://github.com/capitaomorte/sly-slepper
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
;; `sly-slepper` is SLY contrib. See README.md
;;
;;; Code:
(eval-when-compile
  (add-to-list 'load-path "~/Source/Emacs/sly"))
(require 'sly)

(define-sly-contrib sly-slepper
  "Define the `sly-slepper' contrib.
Depends on the `slynk-slepper' ASDF system Insinuates itself
in `sly-editing-mode-hook', i.e. lisp files."
  (:slynk-dependencies slynk-slepper)
  (:on-load (add-hook 'sly-editing-mode-hook 'sly-slepper-mode))
  (:on-unload (remove-hook 'sly-editing-mode-hook 'sly-slepper-mode)))

(defun sly-slepper--sticker-maybe (from to)
  (unless (sly-stickers--stickers-exactly-at from to)
    (sly-stickers--sticker from to)))

(defun sly-slepper (pos)
  "Slepp defun at point POS.  POS defaults to curren point."
  (interactive "d")
  (cl-destructuring-bind (beg end)
      (sly-region-for-defun-at-point pos)
    (cl-loop for result in (sly-eval
                            `(slynk-slepper:slepper
                              ,(buffer-substring-no-properties
                                beg
                                end)))
             for (a . b) = (cl-getf result :source)
             do (save-excursion
                  (sly-slepper--sticker-maybe
                   (+ beg a)
                   (+ beg b)))))
  (message "Done"))

(define-minor-mode sly-slepper-mode
  "A minor mode active when the contrib is active."
  nil nil nil
  (cond (sly-slepper-mode
         )
        (t
         )))

(defvar sly-slepper-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-w") 'sly-slepper)
    map)
  "A keymap accompanying `sly-slepper-mode'.")

;;; Automatically add ourselves to `sly-contribs' when this file is loaded
;;;###autoload
(with-eval-after-load 'sly
  (add-to-list 'sly-contribs 'sly-slepper 'append))

(provide 'sly-slepper)
;;; sly-slepper.el ends here

