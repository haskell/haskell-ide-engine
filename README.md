Home to the community effort to provide a common service backend for any IDE
making use of Haskell in general and GHC in particular.

## Status

This is currently a blank canvas. There has been a fair amount of discussion
about what needs to be done/exist, this is the place where it should all come
together.

Right now there is a google group/mailing list for
[haskell-ide](https://groups.google.com/forum/#!forum/haskell-ide) and an empty
IRC channel at #haskell-ide-engine on freenode.

The initial discussions have taken place in a number of email threads/IRC
channels, this should become the place where these discussions happen in future.

If anyone wants to be a member of this project, contact @alanz or @hvr to add
you to it.

[![Available on Hackage][badge-hackage]][hackage]
[![License BSD3][badge-license]][license]
[![Build Status][badge-travis]][travis]

[badge-travis]: https://travis-ci.org/haskell/haskell-ide.png?branch=master
[travis]: https://travis-ci.org/haskell/haskell-ide
[badge-hackage]: https://img.shields.io/hackage/v/haskell-ide.svg?dummy
[hackage]: https://hackage.haskell.org/package/haskell-ide
[badge-license]: https://img.shields.io/badge/license-BSD3-green.svg?dummy
[license]: https://github.com/haskell/haskell-ide/blob/master/LICENSE

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

According to [#2](https://github.com/haskell/haskell-ide/issues/2) it seems the
consensus is toward (re) using the Idris protocol, as the languages are similar
enough and it offers cross-IDE support already.

The Plugin and IDE layers are very fuzzy at this point, and there has been some
discussion on IRC around it. These layers may well live in a single repository
(this one), as two separate layers or just be a feature of how haskell-ide is
built.

## Documentation

Rather than use the wiki we will put the documentation in the
[docs](https://github.com/haskell/haskell-ide/tree/master/docs) directory here,
so collaborators can either edit or provide pull requests.

  * [Inspirations](https://github.com/haskell/haskell-ide/blob/master/docs/Inspirations.md)
  * [Tools](https://github.com/haskell/haskell-ide/blob/master/docs/Tools.md) that could/should be integrated into haskell-ide
