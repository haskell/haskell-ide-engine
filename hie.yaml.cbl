cradle:
  cabal:
    - path: "./test/dispatcher/"
      component: "haskell-ide-engine:dispatcher-test"

    - path: "./test/functional/"
      component: "haskell-ide-engine:func-test"

    - path: "./test/unit/"
      component: "haskell-ide-engine:unit-test"

    - path: "./test/plugin-dispatcher/"
      component: "haskell-ide-engine:plugin-dispatcher-test"

    - path: "./test/wrapper/"
      component: "haskell-ide-engine:wrapper-test"

    - path: "./hie-plugin-api/"
      component: "lib:hie-plugin-api"

    - path: "./app/MainHie.hs"
      component: "haskell-ide-engine:hie:exe"

    - path: "./app/HieWrapper.hs"
      component: "haskell-ide-engine:hie-wrapper:exe"

    - path: "./"
      component: "lib:haskell-ide-engine"
