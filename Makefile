emacs = emacs24

ifeq (,$(shell which $(emacs) 2> /dev/null))
	emacs = emacs
endif

.PHONY: test
test: test-haskell test-emacs

.PHONY: test-emacs
test-emacs:
	stack exec $(emacs) -- -q --batch -L elisp -l elisp/tests/hie-tests.el -f ert-run-tests-batch-and-exit

.PHONY: test-haskell
test-haskell:
	# stack build --test --pedantic
	# stack complains about deprecations in ghc-mod as an extra dep
	stack build --test

.PHONY: ghci-test
ghci-test:
	stack ghci --main-is=haskell-ide-test
