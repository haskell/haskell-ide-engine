# haskell-ide-engine (HIE)

[![Available on Hackage][badge-hackage]][hackage] [![License BSD3][badge-license]][license] [![Build Status][badge-travis]][travis]

[badge-travis]: https://travis-ci.org/haskell/haskell-ide-engine.png?branch=master
[travis]: https://travis-ci.org/haskell/haskell-ide-engine
[badge-hackage]: https://img.shields.io/hackage/v/haskell-ide-engine.svg?dummy
[hackage]: https://hackage.haskell.org/package/haskell-ide-engine
[badge-license]: https://img.shields.io/badge/license-BSD3-green.svg?dummy
[license]: https://github.com/haskell/haskell-ide-engine/blob/master/LICENSE


This project aims to be the __universal IDE interface__ to __all haskell tools__, proving a __full-featured and easy to query haskell ide-backend__.

-------------

### There is __3 things__ you can do to help:

1. Integrate your tool to HIE  like this : [/hie-example-plugin2/Haskell/Ide/ExamplePlugin2.hs](/hie-example-plugin2/Haskell/Ide/ExamplePlugin2.hs) 
2. Generate IDE Bindings (see [/app/Ide/SublimeText.hs](/app/Ide/SublimeText.hs))
3. Discuss the project with us
     - Register in our [google group mailing list](https://groups.google.com/forum/#!forum/haskell-ide)
     - Join our IRC channel at `#haskell-ide-engine` on `freenode`.
     - Fork this repo and hack as much as you can.
     - Ask @alanz or @hvr to join the project.


---------

:heart: Haskell tooling dream is near, we need your help ! :heart:

### Features: (planned)

 - [ ] cabal / stack project `Configuration` and `Compilation`
 - [ ] Errors Checking, Warnings, Linter, Dead code detection
 - [ ] Refactoring tools, Code beautify, Auto-apply suggestion
 - [ ] Code generation
 - [ ] Run testing Suite, check coverage
 - [ ] Autocompletion
 - [ ] Get type at point
 - [ ] Jump to definition, find Usages, Browse documentation, Generate ctags
 - [ ] REPL


### Is this _again_ an other `ghc-mod` or `ide-backend` like project ?

No:

 > Both the ghc-mod and ide-backend maintainers have agreed to contribute code to this new repository and then rebase the old repos on this. The reason we're using a new repo instead of modifying one of the existing ones is so that the existing projects experience no disruption during this migration process. If this was a new set of people starting a new project without support from existing projects, I'd agree with you. But Alan's reached out to existing players already, which is an important distinction.

This project doesn't start from scratch:

1. See why [we should supersede previous tools](/docs/Challenges.md)
2. Check the [list of existing tools / features ](/docs/Tools.md)
3. See more [other tools / ide for inspiration](/docs/Inspirations.md)

## Architecture

1. __BIOS layer__: Ghc Mod

    ghc-mod stays an AGPL project, and is used for its "awesome sauce" in terms of
    the BIOS functions that it does so well. This interface is
    [straightforward to use](http://alanz.github.io/haskell%20refactorer/2015/10/02/ghc-mod-for-tooling),
    and if a license-constrained user wants to do something else it is also easy to
    replace, if there is strong control of the operating environment.

2. __Plugin layer__:

    A layer providing a point to integrate tools and existing functions (ghci, hlint, etc.).

3. __IDE interfacing layer__:

    This provides a set of logical channels that can be integrated into standard
    IDEs. The details still need to be worked out, but I would imagine something
    like a channel for querying information about a project, one for a ghci session,
    and whatever others are needed. These logical channels can then run over
    whatever transport is appropriate to the specific IDE being integrated.

    According to [#2](https://github.com/haskell/haskell-ide-engine/issues/2) it seems the
    consensus is toward (re) using the Idris protocol, as the languages are similar
    enough and it offers cross-IDE support already.

    The Plugin and IDE layers are very fuzzy at this point, and there has been some
    discussion on IRC around it. These layers may well live in a single repository
    (this one), as two separate layers or just be a feature of how
    haskell-ide-engine is built.

You can see more info [in the docs folder](/docs) at the root of the project, especially:

 - The [Architecture discussion](/docs/Architecture.md)
 - The [Protocol discussion](/docs/Protocol.md)
 - The [Design discussion](/docs/Design.md)


## Documentation

All the documentation is [in the docs folder](/docs) at the root of the project.
