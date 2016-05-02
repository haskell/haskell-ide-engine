#!/usr/bin/env bash
stack $@ build --test \
      --flag haskell-ide-engine:pedantic \
      --flag hie-apply-refact:pedantic \
      --flag hie-base:pedantic \
      --flag hie-docs-generator:pedantic \
      --flag hie-eg-plugin-async:pedantic \
      --flag hie-example-plugin2:pedantic \
      --flag hie-ghc-mod:pedantic \
      --flag hie-ghc-tree:pedantic \
      --flag hie-hare:pedantic \
      --flag hie-plugin-api:pedantic \
