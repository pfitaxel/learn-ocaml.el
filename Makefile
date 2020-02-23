EMACS ?= $(shell if ! command -v emacs; then echo "Emacs binary not found"; exit 1; else echo emacs; fi)
BATCHEMACS := $(EMACS) --batch -Q
BYTECOMPILE := $(BATCHEMACS) --eval "(progn (require 'bytecomp) (require 'package) (setq byte-compile-warnings (remove 'cl-functions byte-compile-warning-types)) (setq byte-compile-error-on-warn t) (batch-byte-compile))"

ELFILE := learn-ocaml.el

ELC := $(ELFILE:.el=.elc)

all: help

help:
	@echo '$$ make elc  # byte-compile $(ELFILE)'
	@echo '$$ make bump v=1.0.0  # Replace version strings with 1.0.0'

bump:
	git diff -p --raw --exit-code || { echo >&2 "*** Please commit beforehand ***"; exit 1; }
	sed -i.bak -e 's/\(^;; Version:\).*$$/\1 $(v)/; s/\(^(defconst learn-ocaml-mode-version\).*$$/\1 "$(v)")/' $(ELFILE)
	! diff -u $(ELFILE).bak $(ELFILE)
	git commit $(ELFILE) -m "Bump $(ELFILE) to version $(v)" -e

%.elc: %.el
	$(BYTECOMPILE) $<

elc: $(ELC)

clean:
	$(RM) $(ELC)

.PHONY: all help bump elc clean
