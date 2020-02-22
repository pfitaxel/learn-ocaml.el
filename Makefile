ELFILE := learn-ocaml.el

all: help

help:
	@echo '$$ make bump v=1.0.0  # Replace version strings with 1.0.0'

bump:
	git diff -p --raw --exit-code || { echo >&2 "*** Please commit beforehand ***"; exit 1; }
	sed -i.bak -e 's/\(^;; Version:\).*$$/\1 $(v)/; s/\(^(defconst learn-ocaml-mode-version\).*$$/\1 "$(v)")/' $(ELFILE)
	! diff -u $(ELFILE).bak $(ELFILE)
	git commit $(ELFILE) -m "Bump $(ELFILE) to version $(v)" -e

.PHONY: all help bump
