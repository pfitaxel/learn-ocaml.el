EMACS ?= emacs
BATCHEMACS := $(EMACS) --batch -Q
BYTECOMPILE := $(BATCHEMACS) --eval "(progn (require 'bytecomp) (setq byte-compile-error-on-warn t) (batch-byte-compile))"

ELFILE := learn-ocaml.el

ELC := $(ELFILE:.el=.elc)

all: elc

help:
	@echo '$$ make elc  # byte-compile $(ELFILE)'
	@echo '$$ make bump v=1.0.0  # Replace version strings with 1.0.0'
	@echo ''
	@echo 'All the make commands below involve "sudo docker".'
	@echo ''
	@echo '$$ make back  # Run a docker backend for interactive ERT tests'
	@echo '$$ make back USE_PASSWD=true  # Same, with mail/passwd enabled'
	@echo '$$ make back LEARNOCAML_IMAGE=ocamlsf/learn-ocaml LEARNOCAML_VERSION=0.12'
	@echo '$$ cat teacher.txt  # initial Teacher token'
	@echo '$$ cat confirm.txt  # mail confirmation URLs (this needs USE_PASSWD=true)'
	@echo ''
	@echo '$$ make emacs  # Run a dockerized emacs for ERT tests'
	@echo '$$ make tests  # Run dockerized ERT tests'
	@echo '$$ make tests USE_PASSWD=true'
	@echo ''
	@echo '$$ make dist-tests  # Alias-of: make back emacs tests'
	@echo '$$ make dist-tests USE_PASSWD=true'
	@echo ''
	@echo '$$ make stop  # Stop the docker backend and/or ERT frontend'

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

back:
	./run_test_backend.sh

emacs:
	./run_emacs_image.sh

tests:
	./run_tests.sh

dist-tests: back emacs tests

stop:
	-./stop_emacs_image.sh
	./stop_test_backend.sh

.PHONY: all help bump elc clean back emacs tests dist-tests stop
