# How to run tests

## Using the Travis CI wrapper

* Install Docker
* Run `cd .. && sudo ./test.sh`

## Using ert within emacs (hacky)

* Install Docker
* Add `learn-ocaml-client` in the `PATH`  
  e.g. by installing OPAM dependencies and compiling `learn-ocaml`
  (`make && make opaminstall`), then doing:  
  `export PATH=$PATH:$HOME/forge/git/learn-ocaml/_opam/bin`
* Finally run:

```bash
cd .../learn-ocaml.el
docker run --name=server -d --rm -p 8080:8080 \
  -v "$PWD/tests/repo:/repository" ocamlsf/learn-ocaml:0.11 build serve
learn-ocaml-client init -s http://localhost:8080 test test
emacs tests/learn-ocaml-tests.el &
```
