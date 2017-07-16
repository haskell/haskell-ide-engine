# haskell-ide-engine
[![License BSD3][badge-license]][license]

[badge-license]: https://img.shields.io/badge/license-BSD3-green.svg?dummy
[license]: https://github.com/haskell/haskell-ide-engine/blob/master/LICENSE


This project aims to be __the universal interface__ to __a growing number of Haskell tools__, providing a __full-featured and easy to query backend__ for editors and IDEs that require Haskell-specific functionality.

__We are currently focusing on using the [Language Server Protocol](https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md) as the interface via which 
we talk to clients.__

## Features:

 - Diagnostics via hlint and GHC warnings/errors
   
   ![Diagnostics](previews/diagnostics.gif?raw=true)

 - Code actions and quick fixes via apply-refact
   
   ![Apply Refact](previews/apply-refact.gif?raw=true)

 - Type information and documentation(via hoogle) on hover

   ![Hover](previews/hover.gif?raw=true)

 - Go to definition
   
   ![Find Def](previews/finddef.gif?raw=true)

 - Document Symbols

   ![Doc Symbols](previews/docsymbols.gif?raw=true)

 - Document Highlight
   
   ![Doc Highlight](previews/dochighlight.gif?raw=true)
   
 - Completion
   
   ![Completion](previews/completion.gif?raw=true)

 - Formatting via brittany

   ![Formatting](previews/formatting.gif?raw=true)

 - Renaming via HaRe

   ![Renaming](previews/rename.gif?raw=true)
 
## Installation 

To install HIE

```bash
git clone https://github.com/haskell/haskell-ide-engine
cd haskell-ide-engine
stack install
```

### Using HIE with vscode

Make sure HIE is installed (see above) and dir stack put the `hie` binary is in your path (usually `~/.local/bin` on linux)

```bash
git clone https://github.com/alanz/vscode-hie-server
cd vscode-hie-server
npm install .
```

Open `vscode-hie-server/` in Visual Studio Code and press `F5` to open a new window with the extension loaded.

### Planned Features

 - [ ] Multiproject and new-build support
 - [ ] HaRe refactorings
 - [ ] More code actions
 - [ ] Cross project/dependency Find Definition
 - [ ] Case splitting, type insertion etc.

### This is *not* yet another [`ghc-mod`](https://github.com/kazu-yamamoto/ghc-mod) or [`ide-backend`](https://github.com/fpco/ide-backend) project

 > Both the ghc-mod and ide-backend maintainers have agreed to contribute code to this new repository and then rebase the old repos on this. The reason we're using a new repo instead of modifying one of the existing ones is so that the existing projects experience no disruption during this migration process. If this was a new set of people starting a new project without support from existing projects, I'd agree with you. But Alan's reached out to existing players already, which is an important distinction.

This project is not started from scratch:

1. See why [we should supersede previous tools](/docs/Challenges.md)
2. Check the [list of existing tools and functionality](/docs/Tools.md)
3. See more [other tools and IDEs for inspiration](/docs/Inspirations.md)


## It's time to join the project!

:heart: Haskell tooling dream is near, we need your help! :heart:

 - Register in [our google group mailing list](https://groups.google.com/forum/#!forum/haskell-ide).
 - Join [our IRC channel](http://webchat.freenode.net/?channels=haskell-ide-engine) at `#haskell-ide-engine` on `freenode`.
 - Fork this repo and hack as much as you can.
 - Ask @alanz or @hvr to join the project.

## Development

Apart from stack you need [cask](https://cask.readthedocs.org/en/latest/) for the emacs tests. You can install it using

```
curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python
```

-------------


## Architecture

Below you find a short overview of the main architectural layers of `haskell-ide-engine`.
For more info have a look in [the docs folder](/docs) at the root of this project, especially:

 - The [Architecture discussion](docs/Architecture.md)
 - The [Protocol discussion](docs/Protocol.md)
 - The [Design discussion](docs/Design.md)

#### 1. BIOS layer

[`ghc-mod`](https://github.com/kazu-yamamoto/ghc-mod) stays an AGPL project,
and is used for its "awesome sauce" in terms of
the BIOS functions that it does so well. This interface is
[straightforward to use](http://alanz.github.io/haskell%20refactorer/2015/10/02/ghc-mod-for-tooling),
and if a license-constrained user wants to do something else it is also easy to
replace, if there is strong control of the operating environment.

#### 2. Plugin layer

A layer providing a point to integrate tools and existing functions, probably
including ghci.

#### 3. IDE interfacing layer

The focus is currently on [LSP](https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md) as the protocol we use to interface with IDEs.

Existing transports are still functional for the time being.

## Documentation

All the documentation is in [the docs folder](/docs) at the root of this project.
