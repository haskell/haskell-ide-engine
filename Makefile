BASEDIR=$(CURDIR)
STACKLOCALBINDIR:=$(shell stack path --local-bin)

all: help
.PHONY: all

## Builds hie for all supported GHC versions (8.2.1, 8.2.2, 8.4.2 and 8.4.3, 8.4.4)
build: hie-8.2.1 hie-8.2.2 \
       hie-8.4.2 hie-8.4.3 hie-8.4.4 \
       hie-8.6.1 hie-8.6.2
.PHONY: build

## Builds hie and hoogle databases for all supported GHC versions
build-all: build build-docs
.PHONY: build-all

GHC := $(shell stack path --compiler-exe)

ghc:
	$(GHC) --version
	@echo GHC
.PHONY: ghc

# ------------------------------------------------------

## Builds hie for GHC version 8.2.1 only
hie-8.2.1: submodules cabal
	stack --stack-yaml=stack-8.2.1.yaml install happy
	stack --stack-yaml=stack-8.2.1.yaml build
	stack --stack-yaml=stack-8.2.1.yaml install                                \
		&& cp '$(STACKLOCALBINDIR)/hie' '$(STACKLOCALBINDIR)/hie-8.2.1'    \
		&& cp '$(STACKLOCALBINDIR)/hie-8.2.1' '$(STACKLOCALBINDIR)/hie-8.2'
.PHONY: hie-8.2.1

## Builds hie for GHC version 8.2.2 only
hie-8.2.2: submodules cabal
	stack --stack-yaml=stack-8.2.2.yaml install happy
	stack --stack-yaml=stack-8.2.2.yaml build
	stack --stack-yaml=stack-8.2.2.yaml install                                \
		&& cp '$(STACKLOCALBINDIR)/hie' '$(STACKLOCALBINDIR)/hie-8.2.2'    \
		&& cp '$(STACKLOCALBINDIR)/hie-8.2.2' '$(STACKLOCALBINDIR)/hie-8.2'
.PHONY: hie-8.2.2

## Builds hie for GHC version 8.4.2 only
hie-8.4.2: submodules cabal
	stack --stack-yaml=stack-8.4.2.yaml build
	stack --stack-yaml=stack-8.4.2.yaml install                                \
		&& cp '$(STACKLOCALBINDIR)/hie' '$(STACKLOCALBINDIR)/hie-8.4.2'    \
		&& cp '$(STACKLOCALBINDIR)/hie-8.4.2' '$(STACKLOCALBINDIR)/hie-8.4'
.PHONY: hie-8.2.2

## Builds hie for GHC version 8.4.3 only
hie-8.4.3: submodules cabal
	stack --stack-yaml=stack-8.4.3.yaml build
	stack --stack-yaml=stack-8.4.3.yaml install                                      \
		&& cp '$(STACKLOCALBINDIR)/hie' '$(STACKLOCALBINDIR)/hie-8.4.3'    \
		&& cp '$(STACKLOCALBINDIR)/hie-8.4.3' '$(STACKLOCALBINDIR)/hie-8.4'
.PHONY: hie-8.4.3

## Builds hie for GHC version 8.4.4 only
hie-8.4.4: submodules cabal
	stack --stack-yaml=stack-8.4.4.yaml build
	stack --stack-yaml=stack-8.4.4.yaml install                                      \
		&& cp '$(STACKLOCALBINDIR)/hie' '$(STACKLOCALBINDIR)/hie-8.4.4'    \
		&& cp '$(STACKLOCALBINDIR)/hie-8.4.4' '$(STACKLOCALBINDIR)/hie-8.4'
.PHONY: hie-8.4.4

## Builds hie for GHC version 8.6.1 only
hie-8.6.1: submodules cabal
	stack --stack-yaml=stack-8.6.1.yaml build
	stack --stack-yaml=stack-8.6.1.yaml install                                      \
		&& cp '$(STACKLOCALBINDIR)/hie' '$(STACKLOCALBINDIR)/hie-8.6.1'    \
		&& cp '$(STACKLOCALBINDIR)/hie-8.6.1' '$(STACKLOCALBINDIR)/hie-8.6'
.PHONY: hie-8.6.1

## Builds hie for GHC version 8.6.2 only
hie-8.6.2: submodules cabal
	stack --stack-yaml=stack-8.6.2.yaml build
	stack --stack-yaml=stack-8.6.2.yaml install                                      \
		&& cp '$(STACKLOCALBINDIR)/hie' '$(STACKLOCALBINDIR)/hie-8.6.2'    \
		&& cp '$(STACKLOCALBINDIR)/hie-8.6.2' '$(STACKLOCALBINDIR)/hie-8.6'
.PHONY: hie-8.6.2

# ------------------------------------------------------

## Updates local git submodules
submodules:
	git submodule update --init
.PHONY: submodules

# ------------------------------------------------------

## Makes sure that Cabal the lib is available for cabal-helper-wapper,
## to speed up project start

## NOTE 1: cabal-helper-wrapper builds with old style cabal build, so
##         must be installed this way.
## NOTE 2: this is temporary, will go away once the new cabal-helper lands.
## NOTE 3: This is needed for stack only projects too
cabal:
	stack install cabal-install
	cabal update
	cabal install Cabal-2.4.1.0 --with-compiler=$(GHC)
.PHONY: cabal

# ------------------------------------------------------

## Builds the Hoogle database for all supported GHC versions
build-docs:
	stack --stack-yaml=stack-8.2.1.yaml exec hoogle generate \
	&& stack --stack-yaml=stack-8.2.2.yaml exec hoogle generate \
	&& stack --stack-yaml=stack-8.4.2.yaml exec hoogle generate \
	&& stack --stack-yaml=stack-8.4.3.yaml exec hoogle generate \
	&& stack --stack-yaml=stack-8.4.4.yaml exec hoogle generate \
	&& stack --stack-yaml=stack-8.6.1.yaml exec hoogle generate \
	&& stack --stack-yaml=stack-8.6.2.yaml exec hoogle generate
.PHONY: build-docs


# ------------------------------------------------------

## Runs hie tests
test: submodules cabal
	stack --stack-yaml=stack-8.2.1.yaml test \
	&& stack --stack-yaml=stack-8.2.2.yaml test \
	&& stack --stack-yaml=stack-8.4.2.yaml test \
	&& stack --stack-yaml=stack-8.4.3.yaml test \
	&& stack --stack-yaml=stack-8.4.4.yaml test \
	&& stack --stack-yaml=stack-8.6.1.yaml test \
	&& stack --stack-yaml=stack-8.6.2.yaml test
.PHONY: test

build-copy-compiler-tool: submodules cabal
	stack --stack-yaml=stack-8.2.1.yaml build --copy-compiler-tool \
	&& stack --stack-yaml=stack-8.2.2.yaml build --copy-compiler-tool \
	&& stack --stack-yaml=stack-8.4.2.yaml build --copy-compiler-tool \
	&& stack --stack-yaml=stack-8.4.3.yaml build --copy-compiler-tool \
	&& stack --stack-yaml=stack-8.4.4.yaml build --copy-compiler-tool \
	&& stack --stack-yaml=stack-8.6.1.yaml build --copy-compiler-tool \
	&& stack --stack-yaml=stack-8.6.2.yaml build --copy-compiler-tool
.PHONY: build-copy-compiler-tool

## Fixes icu related problems in MacOS
icu-macos-fix: icu-macos-fix-install icu-macos-fix-build
.PHONY: icu-macos-fix

icu-macos-fix-install:
	brew install icu4c
.PHONY: icu-macos-fix-install

icu-macos-fix-build:
	stack --stack-yaml=stack-8.2.1.yaml build text-icu     \
	  --extra-lib-dirs=/usr/local/opt/icu4c/lib            \
	  --extra-include-dirs=/usr/local/opt/icu4c/include    \
	&& stack --stack-yaml=stack-8.2.2.yaml build text-icu  \
	  --extra-lib-dirs=/usr/local/opt/icu4c/lib            \
	  --extra-include-dirs=/usr/local/opt/icu4c/include    \
	&& stack --stack-yaml=stack-8.4.2.yaml build text-icu  \
	  --extra-lib-dirs=/usr/local/opt/icu4c/lib            \
	  --extra-include-dirs=/usr/local/opt/icu4c/include    \
	&& stack --stack-yaml=stack-8.4.3.yaml build text-icu  \
	  --extra-lib-dirs=/usr/local/opt/icu4c/lib            \
	  --extra-include-dirs=/usr/local/opt/icu4c/include    \
	&& stack --stack-yaml=stack-8.4.4.yaml build text-icu  \
	  --extra-lib-dirs=/usr/local/opt/icu4c/lib            \
	  --extra-include-dirs=/usr/local/opt/icu4c/include    \
	&& stack --stack-yaml=stack-8.6.1.yaml build text-icu  \
	  --extra-lib-dirs=/usr/local/opt/icu4c/lib            \
	  --extra-include-dirs=/usr/local/opt/icu4c/include    \
	&& stack --stack-yaml=stack-8.6.2.yaml build text-icu  \
	  --extra-lib-dirs=/usr/local/opt/icu4c/lib            \
	  --extra-include-dirs=/usr/local/opt/icu4c/include
.PHONY: icu-macos-fix-build

# ------------------------------------------------------

HIE_GIT_REF:=$(shell git describe --tags)
HIE_DIST_NAME:=hie-${HIE_GIT_REF}-`uname -m`-`uname -s`
HIE_DIST_DIR:=/tmp/${HIE_DIST_NAME}
## Creates a tarball containing all the hie binaries
dist:
	mkdir ${HIE_DIST_DIR}
	stack --stack-yaml=stack-8.2.1.yaml build
	cp .stack-work/install/*/*/8.2.1/bin/hie ${HIE_DIST_DIR}/hie-8.2.1
	stack --stack-yaml=stack-8.2.2.yaml build
	cp .stack-work/install/*/*/8.2.2/bin/hie ${HIE_DIST_DIR}/hie-8.2.2
	stack --stack-yaml=stack-8.4.2.yaml build
	cp .stack-work/install/*/*/8.4.2/bin/hie ${HIE_DIST_DIR}/hie-8.4.2
	stack --stack-yaml=stack-8.4.3.yaml build
	cp .stack-work/install/*/*/8.4.3/bin/hie ${HIE_DIST_DIR}/hie-8.4.3
	stack --stack-yaml=stack-8.4.4.yaml build
	cp .stack-work/install/*/*/8.4.4/bin/hie ${HIE_DIST_DIR}/hie-8.4.4
	stack --stack-yaml=stack-8.6.1.yaml build
	cp .stack-work/install/*/*/8.6.1/bin/hie ${HIE_DIST_DIR}/hie-8.6.1
	stack --stack-yaml=stack-8.6.2.yaml build
	cp .stack-work/install/*/*/8.6.2/bin/hie ${HIE_DIST_DIR}/hie-8.6.2
	cp .stack-work/install/*/*/8.6.2/bin/hie ${HIE_DIST_DIR}/hie
	cp .stack-work/install/*/*/8.6.2/bin/hie-wrapper ${HIE_DIST_DIR}/hie-wrapper
	tar -czf ${HIE_DIST_NAME}.tar.gz -C ${HIE_DIST_DIR} .
	rm -r ${HIE_DIST_DIR}
.PHONY: dist


#######################################################################################################################
# Help task
#######################################################################################################################

# COLORS
GREEN  := $(shell tput -Txterm setaf 2)
YELLOW := $(shell tput -Txterm setaf 3)
WHITE  := $(shell tput -Txterm setaf 7)
RESET  := $(shell tput -Txterm sgr0)

TARGET_MAX_CHAR_NUM=20

## Show help
help:
	@echo ''
	@echo 'Usage:'
	@echo '  ${YELLOW}make${RESET} ${GREEN}<target>${RESET}'
	@echo ''
	@echo 'Targets:'
	@awk '/^[a-zA-Z\-\.\_0-9]+:/ { \
		helpMessage = match(lastLine, /^## (.*)/); \
		if (helpMessage) { \
		  helpCommand = $$1; sub(/:$$/, "", helpCommand); \
			helpMessage = substr(lastLine, RSTART + 3, RLENGTH); \
			printf "  ${YELLOW}%-$(TARGET_MAX_CHAR_NUM)s${RESET} ${GREEN}%s${RESET}\n", helpCommand, helpMessage; \
		} \
	} \
	{ lastLine = $$0 }' $(MAKEFILE_LIST)
