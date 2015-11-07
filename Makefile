
.PHONY: test-emacs
.PHONY: test-haskell
.PHONY: test

test: test-haskell test-emacs

test-emacs:
	stack exec emacs24 -- -q --batch -L elisp -l elisp/tests/haskell-ide-engine-tests.el -f ert-run-tests-batch-and-exit

test-haskell:
	stack build --test --pedantic



