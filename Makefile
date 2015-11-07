
.PHONY: test
test: test-haskell test-emacs

.PHONY: test-emacs
test-emacs:
	stack exec emacs24 -- -q --batch -L elisp -l elisp/tests/haskell-ide-engine-tests.el -f ert-run-tests-batch-and-exit

.PHONY: test-haskell
test-haskell:
	stack build --test --pedantic

.PHONY: ghci-test
ghci-test:
	stack ghci --main-is=haskell-ide-test


