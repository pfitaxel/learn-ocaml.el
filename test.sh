#!/bin/sh
emacs --batch --eval '(message (pp (+ 2 2)))'
emacs --batch -l /learn-ocaml.el
emacs --batch -l ert -l learn-ocaml.el -l test-directory/learn-ocaml-tests.el -f ert-run-tests-batch-and-exit
