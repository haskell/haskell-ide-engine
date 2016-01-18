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
# build with -Werror enabled but run the tests without it because ide-backend picks it up
	stack build --test --flag haskell-ide-engine:pedantic \
					   --flag hie-apply-refact:pedantic \
					   --flag hie-base:pedantic \
					   --flag hie-docs-generator:pedantic \
					   --flag hie-eg-plugin-async:pedantic \
					   --flag hie-example-plugin2:pedantic \
					   --flag hie-ghc-mod:pedantic \
					   --flag hie-ghc-tree:pedantic \
					   --flag hie-hare:pedantic \
					   --flag hie-plugin-api:pedantic

.PHONY: ghci-test
ghci-test:
	stack ghci --main-is=haskell-ide-test
