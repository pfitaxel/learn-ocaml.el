# Makefile --- unit tests

# Copyright (c) 2021  The learn-ocaml.el developers

# This software is free software; you can redistribute it and/or
# modify it under the terms of the MIT License.

# You should have received a copy of the MIT License along with this
# program.  If not, see <https://spdx.org/licenses/MIT>

test:
	emacs --batch -l ert -l ../init-tests.el \
	  -l ../../learn-ocaml.el \
	  -l ../learn-ocaml-tests.el \
	  --eval "(add-to-list 'exec-path \
	    (learn-ocaml-test-client-expected-path))" \
	  --eval "(learn-ocaml-test-use-passwd-auto)" \
	  -l ./runtests.el \
	  -f ert-run-tests-batch-and-exit

.PHONY: test
