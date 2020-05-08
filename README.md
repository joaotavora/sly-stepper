[![MELPA](http://melpa.org/packages/sly-stepper-badge.svg)](http://melpa.org/#/sly-stepper)
[![Build Status](https://travis-ci.org/capitaomorte/sly-stepper.svg?branch=master)](https://travis-ci.org/capitaomorte/sly-stepper)

# A portable Common Lisp stepper interface

`sly-stepper` is an external contrib for [SLY][sly] that was created
from [sly-hello-world](https://github.com/joaotavora/sly-hello-world)

**It's in serious need of a proper README.**

Anyway, it:

* is completely self-contained (doesn't need to be bundled with SLY)

* has both Emacs-Lisp and Common-Lisp counterparts

  See `sly-stepper.el` and `slynk-stepper.lisp`

* has automated [Travis
  tests](https://travis-ci.org/capitaomorte/sly-stepper) already in
  place

  See the file `.travis.yml`. There are some sample unit tests for
  this contrib using SBCL and CCL there.

* is easily added to [MELPA](http://melpa.org)

  Just ask make a
  [pull request to MELPA](https://github.com/milkypostman/melpa/pulls)
  and ask for your recipe to be added to `recipes/`. Use this template:

```lisp
(sly-stepper :fetcher github
                         :repo "capitaomorte/sly-stepper"
                         :files (:defaults
                                 "*.lisp"
                                 "*.asd"))
```


The remainder of this `README.md` file is itself a template for the
one that should be included in a contrib.

## Install from MELPA

Perform the [usual MELPA setup](http://melpa.org) and then select
`sly-stepper` for installation from the package menu or from `M-x
package-install`.

Once it's done, `M-x sly` should now bring up a stepper enabled
SLY.

In `.lisp` files you can now use `M-x sly-stepper` to be informed
about the stepperness of your Lisp.

## Melpa-less install

Since this is an external contrib with both Elisp and Lisp parts,
merely loading the Elisp will have little effect. The contrib has to
be registered in SLY's `sly-contribs` variable for SLY to take care of
loading the Lisp side on demand.

For convenience, the `sly-stepper-autoloads` file takes care
of this automatically. So the following setup in your `~/.emacs` or
`~/.emacs.d/init/el` init file should be enough:

```elisp
;;; regular SLY setup
(setq inferior-lisp-program "/path/to/your/preferred/lisp")
(add-to-list 'load-path "/path/to/sly")
(require 'sly-autoloads)

(add-to-list 'load-path "/path/to/sly-stepper")
(require 'sly-stepper-autoloads)
```

In case you already have SLY loaded and running, you might have to
`M-x sly-setup` and `M-x sly-enable-contrib` to enable it.

`sly-stepper` should now kick in in Lisp buffers.

[sly]: https://github.com/capitaomorte/sly
