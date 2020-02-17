# How to run tests

## Using the Travis CI wrapper

* Install Docker
* Run `cd .. && ./test.sh`

## Using ert within emacs

* Install Docker
* Install `learn-ocaml` using OPAM (`make && make opaminstall`)
* Add `learn-ocaml-client` in the `PATH`, e.g. run in a terminal:
	
		export PATH=$PATH:$HOME/forge/git/learn-ocaml/_opam/bin

* Remark: the Elisp equivalent would be:

		(add-to-list 'exec-path (expand-file-name "~/forge/git/learn-ocaml/_opam/bin"))
		
* Run then:

```bash
cd .../learn-ocaml.el
export LOVERSION=dev
docker pull ocamlsf/learn-ocaml:$LOVERSION
docker run --name=server -d --rm -p 8080:8080 \
  -v "$PWD/tests/repo:/repository" ocamlsf/learn-ocaml:$LOVERSION build serve
learn-ocaml-client init -s http://localhost:8080 test test
emacs tests/learn-ocaml-tests.el &
```

* In the `learn-ocaml-tests.el` buffer, eval using <kbd>C-x C-e </kbd>:

```elisp
;; (progn (load-file "../learn-ocaml.el") (load-file "learn-ocaml-tests.el"))
;; (call-interactively #'ert-run-tests-interactively)
```
