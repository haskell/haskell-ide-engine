emacs = emacs24

ifeq (,$(shell which $(emacs) 2> /dev/null))
	emacs = emacs
endif

.PHONY: test
test: test-haskell test-emacs

.PHONY: test-emacs
test-emacs:
ifeq (,$(shell which cask 2> /dev/null))
	$(error cask not found, please read Readme.md#Development)
else
	./emacs_tests.sh
endif

.PHONY: test-haskell
test-haskell:
# stack build --test --pedantic
# stack complains about deprecations in ghc-mod as an extra dep
# build with -Werror enabled but run the tests without it because ide-backend picks it up
	./pedantic.sh

.PHONY: ghci-test
ghci-test:
	stack ghci --main-is=haskell-ide-test
