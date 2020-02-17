# How to run tests

## Using the Travis CI wrapper

* Install Docker
* Run `cd .. && ./test.sh`

## Using ert within emacs

* Install Docker
* Add `learn-ocaml-client` in the `PATH`  
  e.g. by installing OPAM dependencies and compiling `learn-ocaml`
  (`make && make opaminstall`), then doing:  
  `export PATH=$PATH:$HOME/forge/git/learn-ocaml/_opam/bin`
* Finally run:

```bash
cd .../learn-ocaml.el
docker pull ocamlsf/learn-ocaml:dev
docker run --name=server -d --rm -p 8080:8080 \
  -v "$PWD/tests/repo:/repository" ocamlsf/learn-ocaml:dev build serve
learn-ocaml-client init -s http://localhost:8080 test test
emacs tests/learn-ocaml-tests.el &
```

* In the `learn-ocaml-tests.el` buffer, eval using <kbd>C-x C-e </kbd>:

```elisp
;; (progn (load-file "../learn-ocaml.el") (load-file "learn-ocaml-tests.el"))
;; (call-interactively #'ert-run-tests-interactively)
```
