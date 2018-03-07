BASEDIR=$(CURDIR)

build:
	stack --stack-yaml=stack-8.0.2.yaml install                  \
		&& cp ~/.local/bin/hie ~/.local/bin/hie-8.0.2            \
		&& cp ~/.local/bin/hie ~/.local/bin/hie-8.0              \
	&& stack --stack-yaml=stack-8.2.1.yaml install               \
		&& cp ~/.local/bin/hie ~/.local/bin/hie-8.2.1            \
		&& cp ~/.local/bin/hie-8.2.1 ~/.local/bin/hie-8.2        \
	&& stack --stack-yaml=stack.yaml install                     \
		&& cp ~/.local/bin/hie ~/.local/bin/hie-8.2.2            \
		&& cp ~/.local/bin/hie-8.2.2 ~/.local/bin/hie-8.2
.PHONY: build

hie-8.2.2:
	stack --stack-yaml=stack.yaml install                  \
		&& cp ~/.local/bin/hie ~/.local/bin/hie-8.2.2      \
		&& cp ~/.local/bin/hie-8.2.2 ~/.local/bin/hie-8.2
.PHONY: hie-8.2.2

test:
	stack --stack-yaml=stack-8.0.2.yaml test    \
	&& stack --stack-yaml=stack-8.2.1.yaml test \
	&& stack --stack-yaml=stack.yaml test
.PHONY: test

build-copy-compiler-tool:
	stack --stack-yaml=stack-8.0.2.yaml build --copy-compiler-tool    \
	&& stack --stack-yaml=stack-8.2.1.yaml build --copy-compiler-tool \
	&& stack --stack-yaml=stack.yaml build --copy-compiler-tool
.PHONY: build-copy-compiler-tool

icu-macos-fix:
	brew install icu4c                                     \
	&& stack --stack-yaml=stack-8.0.2.yaml build text-icu  \
         --extra-lib-dirs=/usr/local/opt/icu4c/lib         \
         --extra-include-dirs=/usr/local/opt/icu4c/include \
	&& stack --stack-yaml=stack-8.2.1.yaml build text-icu  \
         --extra-lib-dirs=/usr/local/opt/icu4c/lib         \
         --extra-include-dirs=/usr/local/opt/icu4c/include \
	&& stack --stack-yaml=stack.yaml build text-icu        \
         --extra-lib-dirs=/usr/local/opt/icu4c/lib         \
         --extra-include-dirs=/usr/local/opt/icu4c/include
.PHONY: icu-macos-fix

.PHONY: cabal-project-deps
cabal-project-deps:
	mkdir -p ./.cabal-project-deps
	- (cd ./.cabal-project-deps && git clone https://gitlab.com/alanz/HaRe.git)
	(cd ./.cabal-project-deps/HaRe && git fetch)
	(cd ./.cabal-project-deps/HaRe && git checkout e325975450ce89d790ed3f92de3ef675967d9538)
	- (cd ./.cabal-project-deps && git clone https://github.com/alanz/ghc-mod.git)
	(cd ./.cabal-project-deps/ghc-mod && git fetch)
	(cd ./.cabal-project-deps/ghc-mod && git checkout 47e200a728a575f407ee6f9893d9a1e77b1b5325)
