# learn-ocaml.el

[![Test](https://github.com/pfitaxel/learn-ocaml.el/actions/workflows/test.yml/badge.svg?branch=master)](https://github.com/pfitaxel/learn-ocaml.el/actions/workflows/test.yml)
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
;; (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3") ; see remark below
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
```

> **Remark:** If you have Emacs 26.1 (which is precisely
> [the packaged version in Debian 10](https://packages.debian.org/emacs)),
> you may get the error message `Failed to download 'melpa' archive`
> during the package refresh step. This is a know bug
> ([debbug #34341](https://debbugs.gnu.org/cgi/bugreport.cgi?bug=34341))
> which has been fixed in Emacs 26.3 and 27.1, while a simple workaround
> consists in uncommenting the line
> `(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")` above in your
> `.emacs`.

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

## Developer docs

* **Setup the `learn-ocaml.el` dev environment** by following these refs:
  * [GH: erikmd/tapfa-init.el](https://github.com/erikmd/tapfa-init.el) (opam (2.x) + OCaml (≥ 4.05.0) + GNU Emacs (≥ 25.1) + Tuareg + Merlin)
  * [git-scm.com/download](https://git-scm.com/download) (Git)
  * [Gist: Magit](https://gist.github.com/erikmd/82c4b2a50a77c98e8fe6318530c531b7) (Magit)
  * [Gist: Resources for elisp dev](https://gist.github.com/erikmd/35251ac083e7433f3e780f7eb8856782) (which-key + helpful + edebug-x + Tutorials)
  * [GH: erikmd/docker-examples](https://github.com/erikmd/docker-examples#prérequis--installer-docker-et-docker-compose) (Docker)
* **Use the Makefile to start the test environment**:  
  run `make help` to list the available Makefile commands.
* **Browse the [tests/README.md](./tests/README.md) file** for further details.

## Authors and Acknowledgments

**learn-ocaml.el** has been developed in University Toulouse III by
Erik Martin-Dorel, Manuel Cabarcos Baulina, and Louis Ayroles, thanks to the support
of the [OCaml Software Foundation](http://ocaml-sf.org/).

## License

**learn-ocaml.el** is a free software distributed under the [MIT
License](./LICENSE).
