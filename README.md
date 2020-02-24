# learn-ocaml.el

[![Build Status](https://travis-ci.com/pfitaxel/learn-ocaml.el.svg?branch=master)](https://travis-ci.com/pfitaxel/learn-ocaml.el)
[![MELPA](https://melpa.org/packages/learn-ocaml-badge.svg)](https://melpa.org/#/learn-ocaml)

## Summary

**learn-ocaml.el** is an Emacs frontend for students using
[learn-ocaml](http://ocaml-sf.org/learn-ocaml/).

## Dependencies

### [Emacs](https://www.gnu.org/software/emacs/)

(version `25.1` or later)

### [learn-ocaml-client](https://opam.ocaml.org/packages/learn-ocaml-client/)

(version `0.12` or later)

It can be installed with opam (the OCaml package manager) in a `4.05.0` switch:

* If you already have a `.opam` folder, you should just need to type:

        opam update -y
        opam switch create 4.05.0 ocaml-base-compiler.4.05.0
        eval $(opam env)
        opam install -y learn-ocaml-client

* otherwise, make sure you have [opam 2.0](https://opam.ocaml.org/), then run:

        opam init --auto-setup --yes --compiler=ocaml-base-compiler.4.05.0
        eval $(opam env)
        opam install -y learn-ocaml-client

**Note:** beyond `learn-ocaml-client`, you may want to install a
comprehensive OCaml/Emacs environment (with Tuareg, Merlin, Company):
for details, see <https://github.com/erikmd/tapfa-init.el> (in French)

## Installation (using MELPA)

[MELPA](https://melpa.org/) is a repository of Emacs packages. Skip
this step if you already use MELPA. Otherwise, add the following to
your `.emacs` and restart Emacs:

```elisp
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (add-to-list 'package-archives
               (cons "melpa" (concat proto "://melpa.org/packages/")) t))
(package-initialize)
```

Then, run <kbd>M-x package-refresh-contents RET</kbd> followed by
<kbd>M-x package-install RET learn-ocaml RET</kbd> to install and
byte-compile `learn-ocaml`.


## Usage

Enable the minor mode in any buffer:

<kbd>M-x learn-ocaml-mode RET</kbd>

Follow the instructions regarding the server URL and token (which will
automatically be stored in `~/.config/learnocaml/client.json`).

Open the **`LearnOCaml` menu** or directly start an exercise from the
**\*learn-ocaml-exercise-list\*** buffer.

Then, the `learn-ocaml-mode` will automatically be activated when
opening **.ml** buffers using `tuareg-mode` and the following
*keybindings* will be available:

* `(learn-ocaml-grade)`: <kbd>C-c C-m C-m</kbd> (= <kbd>C-c RET RET</kbd>)

* `(learn-ocaml-display-exercise-list)`: <kbd>C-c C-m C-l</kbd> (= <kbd>C-c RET C-l</kbd>) or <kbd>C-c C-m l</kbd> (= <kbd>C-c RET l</kbd>)

## Known limitations

* When used jointly with [Merlin](https://github.com/ocaml/merlin), if
    the [learn-ocaml](https://github.com/ocaml-sf/learn-ocaml)
    exercise contains a nonempty prelude, **Merlin** may trigger
    errors such as `Unbound value foo` although `foo` is defined in
    the server exercise.  Note that this issue does not impact the
    **learn-ocaml.el** *grading* feature which already works in this
    use case.  But ultimately, we will provide a dedicated support to
    workaround this **Merlin** issue, which thus occurs if the server
    exercise provides some `prelude.ml` and/or `prepare.ml` code.

## Feedback

Bug reports and suggestions are very welcome: feel free to open a new
[issue](https://github.com/pfitaxel/learn-ocaml.el/issues/new) or PR.

## Authors and Acknowledgments

**learn-ocaml.el** has been developed in University Toulouse III by
Erik Martin-Dorel and Manuel Cabarcos Baulina, thanks to the support
of the [OCaml Software Foundation](http://ocaml-sf.org/).

## License

**learn-ocaml.el** is a free software distributed under the [MIT
License](./LICENSE).
