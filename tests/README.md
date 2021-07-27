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

## Note to learn-ocaml.el's CI maintainers

To test learn-ocaml.el w.r.t. another version of learn-ocaml-client:

* Push a new branch in <https://github.com/pfitaxel/docker-emacs-learn-ocaml-client>,
* Wait for the image in <https://hub.docker.com/r/pfitaxel/emacs-learn-ocaml-client>,
* Add a new job line (`LEARNOCAML_VERSION="0.xx"`) in this repo's `.travis.yml`.

# Description of the test-suite

## Unit tests

* `a13_learn-ocaml-file-path` (`ert-deftest`)  
  * Let ex = `/bin/bash`
  * Let dir = `file-name-directory(ex)`
  * Let file = `file-name-nondirectory(ex)`
  * Check ex == `learn-ocaml-file-path(directory-file-name(dir),file)`
  * Check ex == `learn-ocaml-file-path(dir,file)`
  * Check ex == `learn-ocaml-file-path("/dummy",file)`

## Integration tests (`ert-deftest-async`)

**BEWARE** that some tests do `rm -f ~/.config/learnocaml/client.json`

*Note* that the tests use `http://localhost:8080` as server URL.

* `1_learn-ocaml-server-management-test`  
  * Set server (`learn-ocaml-use-metadata-cmd`)
  * Read server (`learn-ocaml-give-server-cmd`)
* `2_learn-ocaml-token-management-test`  
  * Create token from nickname, secret (`learn-ocaml-create-token-cmd`)
  * **TODO** Change nickname(test→Foo)
  * **TODO** Add secret to `server_config.json`
  * Set token (`learn-ocaml-use-metadata-cmd`)
  * Read token (`learn-ocaml-give-token-cmd`)
* `3_learn-ocaml-grade-test`  
  * `rm -f /tmp/learn-ocaml-mode$XXXXXX/demo-results.html`
  * Grade `tests/to_grade.ml` for `demo` (`learn-ocaml-grade-file-cmd`)
  * `grep "Exercise complete" /tmp/learn-ocaml-mode$XXXXXX/demo-results.html` (`should return 0`)
  * `rm -f /tmp/learn-ocaml-mode$XXXXXX/demo-results.html`
* `4_learn-ocaml-download-server-file-test`  
  * `rm -f /tmp/learn-ocaml-mode$XXXXXX/demo.ml`
  * Download `demo.ml` (`learn-ocaml-download-server-file-cmd`)
  * `cat /tmp/learn-ocaml-mode$XXXXXX/demo.ml` (`should return 0`)
  * **TODO** Improve/Split this test, using `diff` or so
* `5_learn-ocaml-download-template-test`  
  * `rm -f /tmp/learn-ocaml-mode$XXXXXX/demo.ml`
  * Download template `demo.ml` (`learn-ocaml-download-template-cmd`)
  * `diff /tmp/learn-ocaml-mode$XXXXXX/demo.ml ./tests/template_demo.ml` (`should return 0`)
  * `rm -f /tmp/learn-ocaml-mode$XXXXXX/demo.ml`
* `6_learn-ocaml-give-exercise-list-test`  
  * Get exercise list (`learn-ocaml-give-exercise-list-cmd`)
  * Read `./tests/exercise_list.json`
  * They should be equal
  * **TODO** Extend this test, using more subdirs?
* `7_learn-ocaml-compute-questions-url-test`  
  * Read server, token (`learn-ocaml-give-server-cmd`, `learn-ocaml-give-token-cmd`)
  * Get description URL of `demo` (`learn-ocaml-compute-questions-url`)
  * `curl -fsS $URL`
  * Read `./tests/expected_description.html`
  * They should be equal
* `8_learn-ocaml-init-another-token`  
  * Create token from nickname, secret (`learn-ocaml-create-token-cmd`)
  * **TODO** Change nickname(test→Foo)
  * **TODO** Add secret to `server_config.json`
  * Test `learn-ocaml-init` {used by `learn-ocaml-login-with-token`} with new token
  * Read new token (`learn-ocaml-give-token-cmd`)
  * They should be equal
* `9_learn-ocaml-init-create-token`  
  * Read token (`learn-ocaml-give-token-cmd`)
  * Test `learn-ocaml-init` {used by `learn-ocaml-login-with-token`} with no token
  * which will call `learn-ocaml-create-token-cmd`
  * Read new token (`learn-ocaml-give-token-cmd`)
  * It should be different
* `a10_learn-ocaml-on-load-test-another-token-no-config`  
  * Read token (`learn-ocaml-give-token-cmd`)
  * `rm -f ~/.config/learnocaml/client.json`
  * Test `learn-ocaml-init` {used by `learn-ocaml-login-with-token`} with URL, initial token
  * Read token (`learn-ocaml-give-token-cmd`)
  * They should be equal
* `a11_learn-ocaml-on-load-test-create-token-no-config`  
  * Read token (`learn-ocaml-give-token-cmd`)
  * `rm -f ~/.config/learnocaml/client.json`
  * Test `learn-ocaml-init` {used by `learn-ocaml-login-with-token`} with URL, nickname, secret
  * **TODO** Change nickname(test→Foo)
  * **TODO** Add secret to `server_config.json`
  * Read token (`learn-ocaml-give-token-cmd`)
  * **FIXME** It should be different
* `a12_learn-ocaml-test-sign-up`  
  * Test `learn-ocaml-client-sign-up-cmd` {used by `learn-ocaml-login-possibly-with-passwd`} with URL, email, passwd, nickname, empty secret
  * **TODO** Change nickname(Test→Foo)
  * **TODO** Add secret ≠ ""
  * Get stdout+stderr
  * Compare with hardcoded string
  * **TODO** Improve this text, using `string-match-p` i/o `string-equa`
* **TODO** Test `learn-ocaml-login-possibly-with-passwd`
