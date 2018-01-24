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
