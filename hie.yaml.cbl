cradle:
  cabal:
    - path: "./hie-plugin-api/Haskell"
      component: "lib:hie-plugin-api"

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

    - path: "./test/utils/"
      component: "haskell-ide-engine:hie-test-utils"

    - path: "./app/MainHie.hs"
      component: "haskell-ide-engine:hie"

    - path: "./app/HieWrapper.hs"
      component: "haskell-ide-engine:hie-wrapper"

    - path: "./"
      component: "lib:haskell-ide-engine"
