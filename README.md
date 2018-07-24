# Haskell IDE Engine
<img src="https://github.com/haskell/haskell-ide-engine/raw/master/logos/HIE_logo_512.png" width="256" style="margin:25px;" align="right"/>

[![License BSD3][badge-license]][license]
[![CircleCI][badge-circleci]][circleci]

[badge-license]: https://img.shields.io/badge/license-BSD3-green.svg?dummy
[license]: https://github.com/haskell/haskell-ide-engine/blob/master/LICENSE
[badge-circleci]: https://img.shields.io/circleci/project/github/haskell/haskell-ide-engine.svg
[circleci]: https://circleci.com/gh/haskell/haskell-ide-engine/


This project aims to be __the universal interface__ to __a growing number of Haskell tools__, providing a __full-featured and easy to query backend__ for editors and IDEs that require Haskell-specific functionality.

__We are currently focusing on using the [Language Server Protocol](https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md) as the interface via which
we talk to clients.__

- [Haskell IDE Engine](#haskell-ide-engine)
    - [Features](#features)
    - [Installation](#installation)
        - Installation with [stack](#installation-with-stack) or [Nix](#installation-with-nix)
        - [ArchLinux](#archlinux)
    - [Configuration](#configuration)
    - [Editor Integration](#editor-integration)
        - Using HIE with [VS Code](#using-hie-with-vs-code), [Sublime Text](#using-hie-with-sublime-text), [Neovim](#using-hie-with-neovim), [Atom](#using-hie-with-atom), [Oni](#using-hie-with-oni), [Emacs](#using-hie-with-emacs) or [Spacemacs](#using-hie-with-spacemacs)
    - [Docs on hover/completion](#docs-on-hovercompletion)
    - [Contributing](#contributing)
        - [Planned Features](#planned-features)
        - [This is *not* yet another `ghc-mod` or `ide-backend` project](#this-is-not-yet-another-ghc-mod-or-ide-backend-project)
        - [It's time to join the project!](#its-time-to-join-the-project)
    - [Architecture](#architecture)
        - [1. BIOS layer](#1-bios-layer)
        - [2. Plugin layer](#2-plugin-layer)
        - [3. IDE interfacing layer](#3-ide-interfacing-layer)
    - [Documentation](#documentation)

## Features

 - Supports plain GHC projects, cabal projects(sandboxed and non sandboxed) and stack projects
 - Fast due to caching of compile info
 - Uses LSP, so should be easy to integrate with a wide selection of editors
 - Diagnostics via hlint and GHC warnings/errors

   ![Diagnostics](https://i.imgur.com/1vqm4eF.gif)

 - Code actions and quick fixes via apply-refact

   ![Apply Refact](https://i.imgur.com/dBrSi5F.gif)

 - Type information and documentation(via haddock) on hover

   ![Hover](https://i.imgur.com/AcvYROv.gif)

 - Jump to definition

   ![Find Def](https://i.imgur.com/kmCU2Bz.gif)

 - List all top level definitions

   ![Doc Symbols](https://i.imgur.com/GErcYqp.gif)

 - Highlight references in document

   ![Doc Highlight](https://i.imgur.com/YLjHs2s.gif)

 - Completion

   ![Completion](https://i.imgur.com/wR6IJ7M.gif)

 - Formatting via brittany

   ![Formatting](https://i.imgur.com/cqZZ8HC.gif)

 - Renaming via HaRe

   ![Renaming](https://i.imgur.com/z03G2a5.gif)

 - Add packages to cabal and hpack package files

   ![Adding package to hpack](https://user-images.githubusercontent.com/2488460/43036067-20ae5964-8cf2-11e8-9951-4fd849b3f735.gif)
   ![Adding import & deps](https://user-images.githubusercontent.com/1387653/40287051-b6f987fe-5c5f-11e8-980f-ed7bfa1b2aec.gif)

 - Typo quick fixes

   ![Quick fixes](https://user-images.githubusercontent.com/2488460/43036093-746ae176-8cf2-11e8-8b2d-59799b21c283.gif)

 - Add missing imports (via hsimport)

   ![Missing imports](https://user-images.githubusercontent.com/2488460/43036113-9bb5d5b0-8cf2-11e8-8e32-20952378cf2b.gif)


## Installation

Note: The version on branch `hie-0.1.0.0` supports GHC 8.0.2. This is
not supported in master.

On linux derivatives you will need to do the equivalent of before building

```bash
sudo apt install libicu-dev libtinfo-dev
```


### Getting the source for GHC 8.2.1, 8.2.2, 8.4.2, 8.4.3

HIE builds from source code, so first,

```bash
$ git clone https://github.com/haskell/haskell-ide-engine --recursive
$ cd haskell-ide-engine
```
### Getting the source for GHC 8.0.2

HIE builds from source code, so first,

```bash
$ git clone https://github.com/haskell/haskell-ide-engine
$ cd haskell-ide-engine
$ git checkout hie-0.1.0.0
$ git submodule update --init
```

### Installation with stack

To install HIE, you need Stack version >= 1.7.1

In order to support both stack and cabal, `hie` requires `cabal-install`
as well.

If it is not already installed, install it, one of the ways is

```bash
stack install cabal-install
```

To install all supported GHC versions, name them as expected by the VS Code
plugin, and also build a local hoogle database, you need the `make` tool (on
Windows, see the further advice below). Use the command:

```bash
make build-all
```

Then add

```json
"languageServerHaskell.useHieWrapper": true
```

to VS Code user settings.

Otherwise, do one of the following.

#### For GHC 8.4.3

Using master

```bash
stack install
```
#### For GHC 8.4.2

Using master

```bash
stack --stack-yaml=stack-8.4.2.yaml install
```

#### For GHC 8.2.2

Using master

```bash
stack --stack-yaml=stack-8.2.2.yaml install
```

Using branch `hie-0.1.0.0`

```bash
stack install
```

#### For GHC 8.2.1

```bash
stack --stack-yaml=stack-8.2.1.yaml install
```

#### For GHC 8.0.2 (only via branch `hie-0.1.0.0`)

```bash
stack --stack-yaml=stack-8.0.2.yaml install
```

### Installation on Windows

#### The `make` tool

If the `make` tool is not already available on your path (in Command Prompt, try
commands `where make` or `stack exec where -- make` to investigate; in
PowerShell, try `where.exe make` or `stack exec where -- make`), it can be added
to the `stack` environment with the command:

```batch
stack exec pacman -- -S make
```

The `make build-all` command is then accessible using the command:

```batch
stack exec make -- build-all
```

For users of [Cygwin](http://www.cygwin.com/), the Cygwin installer also
provides the `make` tool as an option.

Alternatively, the Windows batch file `make-build-all.bat` can substitute for
`make build-all` on systems without the `make` command.

#### Long paths

In order to avoid problems with long paths on Windows you can do the following:

1. Edit the group policy: set "Enable Win32 long paths" to "Enabled". Works
   only for Windows 10

2. Clone the `haskell-ide-engine` to the root of your logical drive (e.g. to
   `E:\hie`)


### Installation with Nix

Follow the instructions at https://github.com/domenkozar/hie-nix


### ArchLinux

An [haskell-ide-engine-git](https://aur.archlinux.org/packages/haskell-ide-engine-git/) package is available on the AUR.

Using [Aura](https://github.com/aurapm/aura):

```
# aura -A haskell-ide-engine-git
```

## Configuration
There are some settings that can be configured via a `settings.json` file:

```json
{
    "languageServerHaskell": {
        "hlintOn": Boolean,
        "maxNumberOfProblems": Number
    }
}
```

- VS Code: These settings will show up in the settings window
- LanguageClient-neovim: Create this file in `$projectdir/.vim/settings.json` or set `g:LanguageClient_settingsPath`

## Editor Integration

Note to editor integrators: there is now a `hie-wrapper` executable, which is installed alongside the `hie` executable.  When this is invoked in the project root directory, it attempts to work out the GHC version used in the project, and then launch the matching `hie` executable.

All of the editor integrations assume that you have already installed HIE (see above) and that `stack` put the `hie` binary in your path (usually `~/.local/bin` on linux and macOS).

### Using HIE with VS Code

Install from
[the VSCode marketplace](https://marketplace.visualstudio.com/items?itemName=alanz.vscode-hie-server), or manually from the repository [vscode-hie-server](https://github.com/alanz/vscode-hie-server).

### Using HIE with Sublime Text

* Make sure HIE is installed (see above) and that the directory stack put the `hie` binary in is in your path
  * (usually `~/.local/bin` on unix)
* Install [LSP](https://packagecontrol.io/packages/LSP) using [Package Control](https://packagecontrol.io/)
* From Sublime Text, press Command+Shift+P and search for Preferences: LSP Settings
* Paste in these settings. Make sure to change the command path to your `hie`

```
"clients": {
  "haskell-ide-engine": {
    "command": ["hie"],
    "scopes": ["source.haskell"],
    "syntaxes": ["Packages/Haskell/Haskell.sublime-syntax"],
    "languageId": "haskell",
  },
},
```

Now open a haskell project with Sublime Text. You should have these features available to you:

1. Errors are underlined in red
2. LSP: Show Diagnostics will show a list of hints and errors
3. LSP: Format Document will prettify the file

### Using HIE with Neovim

As above, make sure HIE is installed. Install and load the neovim plugin
[LanguageClient](https://github.com/autozimu/LanguageClient-neovim). If you use
[vim-plug](https://github.com/junegunn/vim-plug), then you can do this by e.g.
including the following line in the Plug section of your `init.vim`:

```
Plug 'autozimu/LanguageClient-neovim', {
    \ 'branch': 'next',
    \ 'do': './install.sh'
    \ }
```

and issuing a `:PlugInstall` command within neovim.

Finally, make sure that `hie` is included as the language server source for haskell:

```
let g:LanguageClient_serverCommands = {
    ...
    \ 'haskell': ['hie-wrapper'],
    ...
    \ }
```

Since LanguageClient-neovim doesn't start language servers in the project root, it may also be helpful to also specify the project root for hie-wrapper, writing instead:

```
let g:LanguageClient_serverCommands = {
    ...
    \ 'haskell': ['hie-wrapper', '-r', '$YOURROOTHERE'],
    ...
    \ }

```

For asynchronous auto-completion, follow the setup instructions on
[LanguageClient](https://github.com/autozimu/LanguageClient-neovim).

### Using HIE with Atom

Make sure HIE is installed, then install the two Atom packages [atom-ide-ui](https://atom.io/packages/atom-ide-ui) and [ide-haskell-hie](https://atom.io/packages/ide-haskell-hie),

```bash
$ apm install language-haskell atom-ide-ui ide-haskell-hie
```

### Using HIE with Emacs

Install HIE along with the following emacs packages:

[lsp-mode](https://github.com/emacs-lsp/lsp-mode)
[lsp-ui](https://github.com/emacs-lsp/lsp-ui)
[lsp-haskell](https://github.com/emacs-lsp/lsp-haskell)

Make sure to follow the instructions in the README of each of these packages.

### Using HIE with Spacemacs

Install HIE, and then add the following to your `.spacemacs` config,

```lisp
(defun dotspacemacs/layers ()
  "..."
  (setq-default
   ;; ...
   dotspacemacs-configuration-layers
   '(
     lsp
     (haskell :variables ;; Or optionally just haskell without the variables.
              haskell-completion-backend 'ghci
              haskell-process-type 'stack-ghci)
     )
   dotspacemacs-additional-packages '(
      (lsp-haskell :location (recipe :fetcher github :repo "emacs-lsp/lsp-haskell"))
      )
    ;; ...
    ))
```

and then activate [`lsp-haskell`](https://github.com/emacs-lsp/lsp-haskell) in your `user-config` section,

```lisp
(defun dotspacemacs/user-config ()
  "..."
  (require 'lsp-haskell)
  (add-hook 'haskell-mode-hook #'lsp-haskell-enable)
  )
```

Now you should be able to use HIE in Spacemacs. I still recommend checking out [lsp-ui](https://github.com/emacs-lsp/lsp-ui) and [lsp-mode](https://github.com/emacs-lsp/lsp-mode).

### Using HIE with Oni

[Oni](https://www.onivim.io/) (a Neovim GUI) added built-in support for HIE, using stack, in [#1918](https://github.com/onivim/oni/pull/1918/files). If you need to change the configuration for HIE, you can overwrite the following settings in your `~/.config/oni/config.tsx` file (accessible via the command palette and `Configuration: Edit User Config`),

```js
export const configuration = {
  "language.haskell.languageServer.command": "stack",
  "language.haskell.languageServer.arguments": ["exec", "--", "hie"],
  "language.haskell.languageServer.rootFiles": [".git"],
  "language.haskell.languageServer.configuration": {},
}
```

## Docs on hover/completion

HIE supports fetching docs from haddock on hover. It will fallback on using a hoogle db(generally located in ~/.hoogle on linux)
if no haddock documentation is found.

To generate haddock documentation for stack projects:

```bash
$ cd your-project-directory
$ stack haddock --keep-going
```

To enable documentation generation for cabal projects, add the following to your ~/.cabal/config

```
documentation: True
```

To generate a hoogle database that hie can use

```bash
$ cd haskell-ide-engine
$ stack --stack-yaml=<stack.yaml you used to build hie> exec hoogle generate
```

## Contributing

### Planned Features

 - [x] Multiproject support
 - [ ] Project wide references
 - [ ] Cross project find definition
 - [ ] New-build support
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


### It's time to join the project!

:heart: Haskell tooling dream is near, we need your help! :heart:

 - Register in [our google group mailing list](https://groups.google.com/forum/#!forum/haskell-ide).
 - Join [our IRC channel](https://webchat.freenode.net/?channels=haskell-ide-engine) at `#haskell-ide-engine` on `freenode`.
 - Fork this repo and hack as much as you can.
 - Ask @alanz or @hvr to join the project.

## Architecture

Below you find a short overview of the main architectural layers of `haskell-ide-engine`.
For more info have a look in [the docs folder](/docs) at the root of this project, especially:

 - The [Architecture discussion](docs/Architecture.md)
 - The [Protocol discussion](docs/Protocol.md)
 - The [Design discussion](docs/Design.md)

### 1. BIOS layer

[`ghc-mod`](https://github.com/kazu-yamamoto/ghc-mod) stays an AGPL project,
and is used for its "awesome sauce" in terms of
the BIOS functions that it does so well. This interface is
[straightforward to use](http://alanz.github.io/haskell%20refactorer/2015/10/02/ghc-mod-for-tooling),
and if a license-constrained user wants to do something else it is also easy to
replace, if there is strong control of the operating environment.

### 2. Plugin layer

A layer providing a point to integrate tools and existing functions, probably
including ghci.

### 3. IDE interfacing layer

The focus is currently on [LSP](https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md) as the protocol we use to interface with IDEs.

Existing transports are still functional for the time being.

## Documentation

All the documentation is in [the docs folder](/docs) at the root of this project.

## Troubleshooting

### macOS: Got error while processing diagnostics: unable to load package `integer-gmp-1.0.2.0`

[Rename the file](https://github.com/alanz/vscode-hie-server/issues/89#issuecomment-398212122) at `~/.stack/programs/x86_64-osx/ghc-8.4.3/lib/ghc-8.4.3/integer-gmp-1.0.2.0/HSinteger-gmp-1.0.2.0.o` to a temporary name.
[Should be fixed in GHC 8.8.1.](https://ghc.haskell.org/trac/ghc/ticket/15105)
