[![MELPA](http://melpa.org/packages/sly-stepper-badge.svg)](http://melpa.org/#/sly-stepper)
[![Build Status](https://travis-ci.org/capitaomorte/sly-stepper.svg?branch=master)](https://travis-ci.org/capitaomorte/sly-stepper)

# A portable Common Lisp stepper interface

`sly-stepper` is contrib for [SLY, a Common Lisp IDE][sly].

It's the program that accompanies the [paper accepted to the European
Lisp Symposium 2020 Zurich](https://zenodo.org/record/3742759).

It's in need of a good README description, but here's how to get
started.  In any of the two ways described below.

Once that's done, and your SLY is connected you can `M-x sly-stepper`
on a given function to instrument it for stepping, i.e. setting
stickers on every evaluated form of the function.  Then proceed as
normal as you do for [normal SLY sticker
usage](https://joaotavora.github.io/sly/#Stickers).

The command `sly-stepper` is also bound to `C-c C-s P`, for
convenience.  If you make a mistake and set stickers you didn't mean
to, you can clear stickers for the whole defun as you normally do with
`C-u C-c C-s C-s`.

## Normal install

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

## Install from MELPA

Perform the [usual MELPA setup](http://melpa.org) and then select
`sly-stepper` for installation from the package menu or from `M-x
package-install`.

Once it's done, `M-x sly` should now bring up a stepper-enabled
SLY.


[sly]: https://github.com/capitaomorte/sly
