# learn-ocaml.el

[![Build Status](https://travis-ci.com/pfitaxel/learn-ocaml.el.svg?branch=master)](https://travis-ci.com/pfitaxel/learn-ocaml.el)

Summary
-------

**learn-ocaml.el** is an Emacs frontend for students using
[learn-ocaml](http://ocaml-sf.org/learn-ocaml/).

Known limitations
-----------------

* When used jointly with [Merlin](https://github.com/ocaml/merlin), if
    the [learn-ocaml](https://github.com/ocaml-sf/learn-ocaml)
    exercise contains a nonempty prelude, **Merlin** may trigger
    errors such as `Unbound value foo` although `foo` is defined in
    the server exercise.  Note that this issue does not impact the
    **learn-ocaml.el** *grading* feature which already works in this
    use case.  But ultimately, we will provide a dedicated support to
    workaround this **Merlin** issue, which thus occurs if the server
    exercise provides some `prelude.ml` and/or `prepare.ml` code.

Authors and Acknowledgments
---------------------------

**learn-ocaml.el** has been developed in University Toulouse III by
Erik Martin-Dorel and Manuel Cabarcos Baulina, thanks to the support
of the [OCaml Software Foundation](http://ocaml-sf.org/).

License
-------

**learn-ocaml.el** is a free software distributed under the [MIT
License](./LICENSE).
