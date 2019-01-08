BASEDIR=$(CURDIR)
STACKLOCALBINDIR:=$(shell stack path --local-bin)
GHC_VERSIONS= 8.6.3 8.6.2 8.6.1 8.4.4 8.4.3 8.4.2 8.2.2 8.2.1

all: help
.PHONY: all



## Builds hie for all supported GHC versions (8.2.1, 8.2.2, 8.4.2, 8.4.3, 8.4.4, 8.6.1, 8.6.2 and 8.6.3)
build: $(foreach version, $(GHC_VERSIONS), hie-$(version))
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

## Builds hie for GHC version % only
hie-%: submodules cabal
	stack --stack-yaml=stack-$*.yaml install happy
	stack --stack-yaml=stack-$*.yaml build
	stack --stack-yaml=stack-$*.yaml install                                \
		&& cp '$(STACKLOCALBINDIR)/hie' '$(STACKLOCALBINDIR)/hie-$*'    \
		&& cp '$(STACKLOCALBINDIR)/hie-$*' '$(STACKLOCALBINDIR)/hie-$(basename $*)'
.PHONY: hie-%

# ------------------------------------------------------

## Updates local git submodules
submodules:
	git submodule sync --recursive
	git submodule update --init --recursive
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
build-docs: $(foreach version, $(GHC_VERSIONS), build-doc-$(version))
.PHONY: build-docs

build-doc-%:
	stack --stack-yaml=stack-$*.yaml exec hoogle generate

# ------------------------------------------------------

## Runs hie tests
test: $(foreach version, $(GHC_VERSIONS), test-$(version))
.PHONY: test

test-%: submodules cabal
	stack --stack-yaml=stack-$*.yaml test

build-copy-compiler-tool: $(foreach version, $(GHC_VERSIONS), build-copy-compiler-tool-$(version))
.PHONY: build-copy-compiler-tool

build-copy-compiler-tool-%: submodules cabal
	stack --stack-yaml=stack-$*.yaml build --copy-compiler-tool

## Fixes icu related problems in MacOS
icu-macos-fix: icu-macos-fix-install icu-macos-fix-build
.PHONY: icu-macos-fix

icu-macos-fix-install:
	brew install icu4c
.PHONY: icu-macos-fix-install

icu-macos-fix-build: $(foreach version, $(GHC_VERSIONS), icu-macos-fix-build-$(version))
.PHONY: icu-macos-fix-build

icu-macos-fix-build-%:
	stack --stack-yaml=stack-$*.yaml build text-icu \
	  --extra-lib-dirs=/usr/local/opt/icu4c/lib \
	  --extra-include-dirs=/usr/local/opt/icu4c/include

# ------------------------------------------------------

HIE_GIT_REF:=$(shell git describe --tags)
HIE_DIST_NAME:=hie-${HIE_GIT_REF}-$(shell uname -m)-$(shell uname -s)
HIE_DIST_DIR:=/tmp/${HIE_DIST_NAME}

## Creates a tarball containing all the hie binaries
dist: $(foreach version, $(GHC_VERSIONS), dist-$(version))
	cp .stack-work/install/*/*/$(firstword $(GHC_VERSIONS))/bin/hie ${HIE_DIST_DIR}
	cp .stack-work/install/*/*/$(firstword $(GHC_VERSIONS))/bin/hie-wrapper ${HIE_DIST_DIR}
	tar -czf ${HIE_DIST_NAME}.tar.gz -C ${HIE_DIST_DIR} .
	rm -r ${HIE_DIST_DIR}

dist-%:
	mkdir -p ${HIE_DIST_DIR}
	stack --stack-yaml=stack-$*.yaml build
	cp .stack-work/install/*/*/$*/bin/hie ${HIE_DIST_DIR}
	mv ${HIE_DIST_DIR}/hie ${HIE_DIST_DIR}/hie-$*
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
	@awk '/^[a-zA-Z\-\.\_0-9%]+:/ { \
		helpMessage = match(lastLine, /^## (.*)/); \
		if (helpMessage) { \
		  helpCommand = $$1; sub(/:$$/, "", helpCommand); \
		  helpMessage = substr(lastLine, RSTART + 3, RLENGTH); \
		  hasWildcard = match(helpCommand, /%/); \
		  if (hasWildcard) {\
		    split("$(GHC_VERSIONS)", versions); \
		    for (version in versions) { \
		      expandedCommand = helpCommand; sub(/%/, versions[version], expandedCommand);\
		      expandedMessage = helpMessage; sub(/%/, versions[version], expandedMessage);\
		      printf "  ${YELLOW}%-$(TARGET_MAX_CHAR_NUM)s${RESET} ${GREEN}%s${RESET}\n", expandedCommand, expandedMessage; \
	            } \
		  } else { \
		    printf "  ${YELLOW}%-$(TARGET_MAX_CHAR_NUM)s${RESET} ${GREEN}%s${RESET}\n", helpCommand, helpMessage; \
	          } \
		} \
	} \
	{ lastLine = $$0 }' $(MAKEFILE_LIST)
