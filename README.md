# haskell-ide-engine
[![Available on Hackage][badge-hackage]][hackage]
[![License BSD3][badge-license]][license]
[![Build Status][badge-travis]][travis]

[badge-travis]: https://travis-ci.org/haskell/haskell-ide-engine.png?branch=master
[travis]: https://travis-ci.org/haskell/haskell-ide-engine
[badge-hackage]: https://img.shields.io/hackage/v/haskell-ide-engine.svg?dummy
[hackage]: https://hackage.haskell.org/package/haskell-ide-engine
[badge-license]: https://img.shields.io/badge/license-BSD3-green.svg?dummy
[license]: https://github.com/haskell/haskell-ide-engine/blob/master/LICENSE


This project aims to be the universal `IDE interface` to all `haskell tools`.
It will adapt to all ide by offering a `command line interface` and a `webservice`:

 - [ ] cabal / stack project `Configuration` and `Compilation`
 - [ ] Errors Checking, Warnings, Linter, Dead code detection,
 - [ ] Refactoring tools, Code beautify, Auto-apply suggestions
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

1. See why [we should superseed previous tools](/docs/Challenges)
2. See the [list of existting tools / features ](/docs/Tools.md)

### Architecture
Right now there is a google group/mailing list for
[haskell-ide](https://groups.google.com/forum/#!forum/haskell-ide) and an
IRC channel at #haskell-ide-engine on freenode.

If anyone wants to be a member of this project, contact @alanz or @hvr to add
you to it.


## Next steps

After lots of discussion around layers and licensing, it looks like the way
forward is as follows

### BIOS layer

ghc-mod stays an AGPL project, and is used for its "awesome sauce" in terms of
the BIOS functions that it does so well. This interface is
[straightforward to use](http://alanz.github.io/haskell%20refactorer/2015/10/02/ghc-mod-for-tooling),
and if a license-constrained user wants to do something else it is also easy to
replace, if there is strong control of the operating environment.

### Plugin layer

A layer providing a point to integrate tools and existing functions, probably
including ghci.

### IDE interfacing layer

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

## Documentation

Rather than use the wiki we will put the documentation in the
[docs](https://github.com/haskell/haskell-ide-engine/tree/master/docs) directory here,
so collaborators can either edit or provide pull requests.

  * [Inspirations](https://github.com/haskell/haskell-ide-engine/blob/master/docs/Inspirations.md)
  * [Tools](https://github.com/haskell/haskell-ide-engine/blob/master/docs/Tools.md) that could/should be integrated into haskell-ide-engine
