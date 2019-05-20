#!/bin/sh

# stack test haskell-ide-engine:func-test --test-arguments "--failure-report .hspec-failures --rerun"
# stack test haskell-ide-engine:func-test --test-arguments "--match \"completes ghc options pragma values\""
#stack test haskell-ide-engine:func-test --test-arguments "--match \"behaviour on malformed projects\""

# stack --stack-yaml=stack-8.6.4.yaml test haskell-ide-engine:func-test --test-arguments "--match \"completes imports\""
# stack --stack-yaml=stack.yaml test haskell-ide-engine:func-test --test-arguments "--match \"completes pragmas no close\""

# stack --stack-yaml=stack-8.6.4.yaml test haskell-ide-engine:unit-test --test-arguments "--match \"ApplyRefactPlugin\""

stack --stack-yaml=stack.yaml test haskell-ide-engine:func-test --test-arguments "--match \"/FunctionalCodeActions/code actions/hlint suggestions/provides 3.8 code actions/\""

  # To rerun use: --match "/FunctionalCodeActions/code actions/hlint suggestions/falls back to pre 3.8 code actions/"
  # To rerun use: --match "/FunctionalCodeActions/code actions/hlint suggestions/runs diagnostics on save/"
