BASEDIR=$(CURDIR)
STACKLOCALBINDIR:=$(shell stack path --local-bin)

build: hie-8.2.1 hie-8.2.2 hie-8.4.2 hie-8.4.3
.PHONY: build

build-all: build build-docs
.PHONY: build-all

submodules:
	git submodule update --init
.PHONY: submodules

# ------------------------------------------------------

hie-8.2.1: submodules
	stack --stack-yaml=stack-8.2.1.yaml install happy
	stack --stack-yaml=stack-8.2.1.yaml install                                \
		&& cp '$(STACKLOCALBINDIR)/hie' '$(STACKLOCALBINDIR)/hie-8.2.1'    \
		&& cp '$(STACKLOCALBINDIR)/hie-8.2.1' '$(STACKLOCALBINDIR)/hie-8.2'
.PHONY: hie-8.2.1

hie-8.2.2: submodules
	stack --stack-yaml=stack-8.2.2.yaml install happy
	stack --stack-yaml=stack-8.2.2.yaml install                                \
		&& cp '$(STACKLOCALBINDIR)/hie' '$(STACKLOCALBINDIR)/hie-8.2.2'    \
		&& cp '$(STACKLOCALBINDIR)/hie-8.2.2' '$(STACKLOCALBINDIR)/hie-8.2'
.PHONY: hie-8.2.2

hie-8.4.2: submodules
	stack --stack-yaml=stack-8.4.2.yaml install                                \
		&& cp '$(STACKLOCALBINDIR)/hie' '$(STACKLOCALBINDIR)/hie-8.4.2'    \
		&& cp '$(STACKLOCALBINDIR)/hie-8.4.2' '$(STACKLOCALBINDIR)/hie-8.4'
.PHONY: hie-8.2.2

hie-8.4.3: submodules
	stack --stack-yaml=stack.yaml install                                      \
		&& cp '$(STACKLOCALBINDIR)/hie' '$(STACKLOCALBINDIR)/hie-8.4.3'    \
		&& cp '$(STACKLOCALBINDIR)/hie-8.4.3' '$(STACKLOCALBINDIR)/hie-8.4'
.PHONY: hie-8.4.3

# ------------------------------------------------------

build-docs:
	stack --stack-yaml=stack-8.2.1.yaml exec hoogle generate \
	&& stack --stack-yaml=stack-8.2.2.yaml exec hoogle generate \
	&& stack --stack-yaml=stack-8.4.2.yaml exec hoogle generate \
	&& stack --stack-yaml=stack.yaml exec hoogle generate
.PHONY: build-docs


# ------------------------------------------------------

test: submodules
	stack --stack-yaml=stack-8.2.1.yaml test \
	&& stack --stack-yaml=stack-8.2.2.yaml test \
	&& stack --stack-yaml=stack-8.4.2.yaml test \
	&& stack --stack-yaml=stack.yaml test
.PHONY: test

build-copy-compiler-tool: submodules
	stack --stack-yaml=stack-8.2.1.yaml build --copy-compiler-tool \
	&& stack --stack-yaml=stack-8.2.2.yaml build --copy-compiler-tool \
	&& stack --stack-yaml=stack-8.4.2.yaml build --copy-compiler-tool \
	&& stack --stack-yaml=stack.yaml       build --copy-compiler-tool
.PHONY: build-copy-compiler-tool

icu-macos-fix: icu-macos-fix-install icu-macos-fix-no-brew
.PHONY: icu-macos-fix

icu-macos-fix-install:
	brew install icu4c
.PHONY: icu-macos-fix-install

icu-macos-fix-no-brew:
	stack --stack-yaml=stack-8.2.1.yaml build text-icu     \
	  --extra-lib-dirs=/usr/local/opt/icu4c/lib            \
	  --extra-include-dirs=/usr/local/opt/icu4c/include    \
	&& stack --stack-yaml=stack-8.2.2.yaml build text-icu  \
         --extra-lib-dirs=/usr/local/opt/icu4c/lib         \
         --extra-include-dirs=/usr/local/opt/icu4c/include \
	&& stack --stack-yaml=stack-8.4.2.yaml build text-icu  \
         --extra-lib-dirs=/usr/local/opt/icu4c/lib         \
         --extra-include-dirs=/usr/local/opt/icu4c/include \
	&& stack --stack-yaml=stack.yaml build text-icu        \
         --extra-lib-dirs=/usr/local/opt/icu4c/lib         \
         --extra-include-dirs=/usr/local/opt/icu4c/include
.PHONY: icu-macos-fix-no-brew

