(require 'sly)
(require 'sly-slepper)
(require 'sly-tests "lib/sly-tests")

(define-sly-ert-test sly-slepper-basic-test ()
  (with-temp-buffer
    (lisp-mode)
    (should sly-slepper-mode)
    (should-not sly-slepper--last-reported-feature)
    (sly-slepper)
    (should sly-slepper--last-reported-feature)
    (should (keywordp sly-slepper--last-reported-feature))))
