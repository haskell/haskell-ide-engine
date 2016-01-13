emacs = emacs24

ifeq (,$(shell which $(emacs) 2> /dev/null))
	emacs = emacs
endif

.PHONY: test
test: build-haskell test-haskell test-emacs

.PHONY: test-emacs
test-emacs:
	stack exec $(emacs) -- -q --batch -L elisp -l elisp/tests/hie-tests.el -f ert-run-tests-batch-and-exit

.PHONY: build-haskell
build-haskell:
	@STACK_YAML=stack_test.yaml stack build --test --no-run-tests

.PHONY: test-haskell
test-haskell:
# stack build --test --pedantic
# stack complains about deprecations in ghc-mod as an extra dep
# build with -Werror enabled but run the tests without it because ide-backend picks it up
	stack build --test

.PHONY: ghci-test
ghci-test:
	stack ghci --main-is=haskell-ide-test
