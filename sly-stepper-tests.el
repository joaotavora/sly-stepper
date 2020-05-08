(require 'sly)
(require 'sly-stepper)
(require 'sly-tests "lib/sly-tests")

(define-sly-ert-test sly-stepper-basic-test ()
  (with-temp-buffer
    (lisp-mode)
    (should sly-stepper-mode)
    (should-not sly-stepper--last-reported-feature)
    (sly-stepper)
    (should sly-stepper--last-reported-feature)
    (should (keywordp sly-stepper--last-reported-feature))))
