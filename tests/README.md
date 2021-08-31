# How to run tests

## Using only Docker containers

* (Useful to preserve `~/.config/learnocaml/client.json` on the host.)
* Install Docker
* Then, run:
  * `cd ..`
  * `make dist-tests` or `make dist-tests USE_PASSWD=true`
  * `make stop`

## Using ert within emacs

* Install Docker
* Then, run:
  * `cd ..`
  * `make back` or `make back USE_PASSWD=true`
* Ensure `learn-ocaml` has been cloned in a sibling directory, e.g.:
  * `~/forge/git/learn-ocaml.el`
  * `~/forge/git/learn-ocaml`
* Build `learn-ocaml` using OPAM (`make && make opaminstall`)
* Open a `runtests.el` file, eval using <kbd>C-x C-e </kbd>:

```elisp
;; (progn (load-file "../../learn-ocaml.el") (load-file "../learn-ocaml-tests.el") (load-file "./runtests.el"))
;; (add-to-list 'exec-path (learn-ocaml-test-client-expected-path))
;; (progn (learn-ocaml-test-use-passwd-auto)(learn-ocaml-test-dir))
;; (call-interactively #'ert-run-tests-interactively)
```

## Note to learn-ocaml.el's CI maintainers

To test learn-ocaml.el w.r.t. another version of learn-ocaml-client:

* Add the image in <https://hub.docker.com/r/pfitaxel/emacs-learn-ocaml-client>
  * Either by pushing a new branch in
    <https://github.com/pfitaxel/docker-emacs-learn-ocaml-client>
	(for existing learn-ocaml releases)
  * Or by committing in a continuously-deployed dev branch such as
    <https://github.com/pfitaxel/learn-ocaml/blob/oauth-moodle-dev/.github/workflows/deploy-oauth-moodle.yml>
* Add a matrix item in `.github/workflows/test.yml` (`client_version: "0.xx"`).

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

**BEWARE** all tests do `rm -f ~/.config/learnocaml/client.json`

To avoid this, you can use the dockerized version, `make tests`.  
(*TODO* or, one might use `learn-ocaml-client --local` instead...)

*Note* that the tests use `http://localhost:8080` as server URL.

### ITs in 001-common

* `1_learn-ocaml-server-management-test` (*001-common*)  
  * Set server (`learn-ocaml-use-metadata-cmd`)
  * Read server (`learn-ocaml-give-server-cmd`)
* `3_learn-ocaml-grade-test` (*001-common*)  
  * `rm -f /tmp/learn-ocaml-mode$XXXXXX/demo-results.html`
  * Grade `tests/to_grade.ml` for `demo` (`learn-ocaml-grade-file-cmd`)
  * `grep "Exercise complete" /tmp/learn-ocaml-mode$XXXXXX/demo-results.html` (`should return 0`)
  * `rm -f /tmp/learn-ocaml-mode$XXXXXX/demo-results.html`
* `4_learn-ocaml-download-server-file-test` (*001-common*)  
  * `rm -f /tmp/learn-ocaml-mode$XXXXXX/demo.ml`
  * Grade `tests/to_grade.ml` for `demo` (`learn-ocaml-grade-file-cmd`)
  * `rm -f /tmp/learn-ocaml-mode$XXXXXX/demo-results.html`
  * `rm /tmp/learn-ocaml-mode$XXXXXX/demo.ml`
  * Download `demo.ml` (`learn-ocaml-download-server-file-cmd`)
  * `cat /tmp/learn-ocaml-mode$XXXXXX/demo.ml` (`should return 0`)
  * **TODO** Improve/Split this test, using `diff` or so
  * `rm /tmp/learn-ocaml-mode$XXXXXX/demo.ml`
* `5_learn-ocaml-download-template-test` (*001-common*)  
  * `rm -f /tmp/learn-ocaml-mode$XXXXXX/demo.ml`
  * Download template `demo.ml` (`learn-ocaml-download-template-cmd`)
  * `diff /tmp/learn-ocaml-mode$XXXXXX/demo.ml ./tests/template_demo.ml` (`should return 0`)
  * `rm /tmp/learn-ocaml-mode$XXXXXX/demo.ml`
* `6_learn-ocaml-give-exercise-list-test` (*001-common*)  
  * Get exercise list (`learn-ocaml-give-exercise-list-cmd`)
  * Read `./tests/exercise_list.json`
  * They should be equal
  * **TODO** Extend this test, using more subdirs?
* `7_learn-ocaml-compute-questions-url-test` (*001-common*)  
  * Read server, token (`learn-ocaml-give-server-cmd`, `learn-ocaml-give-token-cmd`)
  * Get description URL of `demo` (`learn-ocaml-compute-questions-url`)
  * `curl -fsS $URL`
  * Read `./tests/expected_description.html`
  * They should be equal
* `a10_learn-ocaml-on-load-test-another-token-no-config` (*001-common*)  
  * Read token (`learn-ocaml-give-token-cmd`)
  * `rm -f ~/.config/learnocaml/client.json`
  * Test `learn-ocaml-init` {used by `learn-ocaml-login-with-token`} with URL, initial token
  * Read token (`learn-ocaml-give-token-cmd`)
  * They should be equal

### ITs in 002-use-token context

* `2_learn-ocaml-token-management-test` (**002-use-token**)  
  * Create token from nickname, secret (`learn-ocaml-create-token-cmd`)
  * Set token (`learn-ocaml-use-metadata-cmd`)
  * Read token (`learn-ocaml-give-token-cmd`)
* `8_learn-ocaml-init-another-token` (**002-use-token**)  
  * Create token from nickname, secret (`learn-ocaml-create-token-cmd`)
  * Test `learn-ocaml-init` {used by `learn-ocaml-login-with-token`} with new token
  * Read new token (`learn-ocaml-give-token-cmd`)
  * They should be equal
* `9_learn-ocaml-init-create-token` (**002-use-token**)  
  * Read token (`learn-ocaml-give-token-cmd`)
  * Test `learn-ocaml-init` {used by `learn-ocaml-login-with-token`} with no token, secret
  * which will call `learn-ocaml-create-token-cmd`
  * Read new token (`learn-ocaml-give-token-cmd`)
  * It should be different
* `a11_learn-ocaml-on-load-test-create-token-no-config` (**002-use-token**)  
  * Read token (`learn-ocaml-give-token-cmd`)
  * `rm -f ~/.config/learnocaml/client.json`
  * Test `learn-ocaml-init` {used by `learn-ocaml-login-with-token`} with URL, nickname, secret
  * Read token (`learn-ocaml-give-token-cmd`)
  * **FIXME** It should be different

### ITs in 003-use-passwd context

* `a12_learn-ocaml-test-sign-up` (**003-use-passwd**)  
  * Test `learn-ocaml-client-sign-up-cmd` {used by `learn-ocaml-login-possibly-with-passwd`} with URL, email, passwd, nickname, empty secret
  * **TODO** Add secret ≠ ""
  * Get stdout+stderr
  * Compare with hardcoded string
  * **TODO** Extend this test?
* **TODO** Test `learn-ocaml-login-possibly-with-passwd`

### Guidelines for adding integration tests

* Document the test scenario (in this document, above)
* Write an `(ert-deftest-async)` in the appropriate `000-context/runtests.el`
* If the function requires to be logged (with the "cookie file"),
  use the fixture `(learn-ocaml-test-run-with :before-login-teacher t :body _)`,
  or (**TODO**) use `(learn-ocaml-test-run-with :before-signup t :body _)`.
