# Haskell IDE Engine (HIE)
<img src="https://github.com/haskell/haskell-ide-engine/raw/master/logos/HIE_logo_512.png" width="256" style="margin:25px;" align="right"/>

[![License BSD3][badge-license]][license]
[![CircleCI][badge-circleci]][circleci]
[![AppVeyor][badge-appveyor]][appveyor]

[badge-license]: https://img.shields.io/badge/license-BSD3-green.svg?dummy
[license]: https://github.com/haskell/haskell-ide-engine/blob/master/LICENSE
[badge-circleci]: https://img.shields.io/circleci/project/github/haskell/haskell-ide-engine.svg
[circleci]: https://circleci.com/gh/haskell/haskell-ide-engine/
[badge-appveyor]: https://ci.appveyor.com/api/projects/status/6hit7mxvgdrao3q0?svg=true
[appveyor]: https://ci.appveyor.com/project/Bubba/haskell-ide-engine-74xec


This project aims to be __the universal interface__ to __a growing number of Haskell tools__, providing a __full-featured and easy to query backend__ for editors and IDEs that require Haskell-specific functionality.

__We are currently focusing on using the [Language Server Protocol](https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md) as the interface via which
we talk to clients.__

- [Haskell IDE Engine](#haskell-ide-engine)
    - [Features](#features)
    - [Installation](#installation)
        - [stack on Linux](#installation-with-stack-on-linux)
        - [stack on Windows](#installation-with-stack-on-windows)
        - [macOS](#installation-on-macos)
        - [Nix](#installation-with-nix)
        - [ArchLinux](#installation-on-archlinux)
        - [Shake](#installation-with-shake)
    - [Configuration](#configuration)
    - [Editor Integration](#editor-integration)
        - Using HIE with [VS Code](#using-hie-with-vs-code), [Sublime Text](#using-hie-with-sublime-text), [Vim/Neovim](#using-hie-with-vim-or-neovim), [Atom](#using-hie-with-atom), [Oni](#using-hie-with-oni), [Emacs](#using-hie-with-emacs), [Spacemacs](#using-hie-with-spacemacs) or [Spacemacs+Nix](#using-hie-with-spacemacs-on-nix-based-projects)
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
    - [Troubleshooting](#troubleshooting)

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

### Installation with stack on Linux

To install HIE, you need stack version >= 1.7.1.

HIE builds from source code, so there's a couple of extra steps. 

#### Linux pre-requirements

On Linux you will need install a couple of extra libraries (for Unicode ([ICU](http://site.icu-project.org/)) and [NCURSES](https://www.gnu.org/software/ncurses/)):

**Debian/Ubuntu**: 

```bash
sudo apt install libicu-dev libtinfo-dev libgmp-dev
```
**Fedora**:

```bash
sudo dnf install libicu-devel ncurses-devel
```
**ArchLinux**: see [below](#installation-on-archlinux).

#### Download the source code

```bash
git clone https://github.com/haskell/haskell-ide-engine --recursive
cd haskell-ide-engine
```

In order to support both stack and cabal, HIE requires `cabal-install`
as well. If it is not already installed, install it and update its package list:

```bash
stack install cabal-install
cabal update
```

#### Choose your GHC version

The GHC version you are going to install HIE for depends on which version of GHC you are using for your project. If you don't have a current project there are two potential options:

1. The Nightly GHC version ([currently](https://www.stackage.org/nightly) 8.6.3)
2. The LTS GHC version (which is [currently](https://www.stackage.org/lts) 8.4.4)

By default in a stack project you will get the LTS version.

You can check which version of ghc you are using in your project by running the following at the root of your project:

```bash
stack ghc -- --version
```

You can install an specific version or [all available GHC versions](#install-all-available-ghc-versions).

#### Install a specific GHC version 8.2.1 - 8.6.3

We will use the `make` tools here to wrap `stack install`. The preferred installation mechanism is via `make`, as it makes sure the repo is synced, installs the required cabal libraries if missing, and makes copies of the executables with suffixes to be able to tell them apart.

Install **Nightly** (and hoogle docs):

```bash
make hie-8.6.3
make build-doc-8.6.3
```

Install **LTS** (and hoogle docs):

```bash
make hie-8.4.4
make build-doc-8.4.4
```

This step can take more than 30 minutes, so grab a coffee and please be patient!

The available versions depend on the `stack-*.yaml` config files in the `haskell-ide-engine` directory.

#### For GHC 8.0.2

This is no longer supported on the HIE `master` branch, so you must switch to the `hie-0.1.0.0` branch:

```bash
git checkout hie-0.1.0.0
git submodule update --init
```
Then you can run `stack install`:

```bash
stack --stack-yaml=stack-8.0.2.yaml install
```

#### Install *all* available GHC versions

This is the simplest approach as it will install all GHC versions to match against any project versions you might have.

*Warning*: Requires 20+ GB of space and potentially more than 2 hours to install, so please be patient!

This will:

* install all supported GHC versions (8.2.1 - 8.6.3)
* name them as expected by the VS Code plugin
* build local hoogle docs for each version

For this you need the `make` tool (on Windows, see the further advice below). Use the command:

```bash
make build-all
```

Then add

```json
"languageServerHaskell.useCustomHieWrapper": true,
"languageServerHaskell.useCustomHieWrapperPath": "hie-wrapper",
```

to VS Code user settings.

### Installation with stack on Windows

To install HIE, you need stack version >= 1.7.1.

#### Download the source code

```bash
git clone https://github.com/haskell/haskell-ide-engine --recursive
cd haskell-ide-engine
```

In order to support both stack and cabal, HIE requires `cabal-install`
as well. If it is not already installed, install it and update its package list:

```bash
stack install cabal-install
cabal update
```

#### Install *all* available GHC versions

*Warning*: Requires 20+ GB of space and potentially more than 2 hours to install, so please be patient!

This will:

* install all supported GHC versions (8.2.1 - 8.6.3)
* name them as expected by the VS Code plugin
* build local hoogle docs for each version

`make` doesn't work on Windows due to several UNIX-specific things, such
as the `cp` command or extensionless executable names. Instead, a PowerShell
script is provided specifically for this purpose:

**PowerShell:**

```
./build-all.ps1
```

**cmd.exe:**

```
powershell -ExecutionPolicy RemoteSigned -c ./build-all.ps1
```

#### Long paths

In order to avoid problems with long paths on Windows you can do the following:

1. Edit the group policy: set "Enable Win32 long paths" to "Enabled" (Works
   only for Windows 10).

2. Clone the `haskell-ide-engine` to the root of your logical drive (e.g. to
   `C:\hie`)

### Installation on macOS

Download the pre-built binaries from the [releases page](https://github.com/haskell/haskell-ide-engine/releases), and copy/symlink them into `/usr/local/bin` (or somewhere else in your $PATH):

```bash
ln -s hie-bin-dir/hie* /usr/local/bin/
```

Alternatively, you can install from source with `make build`. 

### Installation with Nix

Follow the instructions at https://github.com/domenkozar/hie-nix


### Installation on ArchLinux

An [haskell-ide-engine-git](https://aur.archlinux.org/packages/haskell-ide-engine-git/) package is available on the AUR.

Using [Aura](https://github.com/aurapm/aura):

```
# aura -A haskell-ide-engine-git
```

### Installation with Shake

Experimental build script for HIE. Feedback is appreciated.
Uses the [shake](https://shakebuild.com/) build system for predictable builds.
The build script is platform independent and the only prerequisites are that `git` and `stack` are installed. The dependency on `make` and other linux specific commands has been dropped.

Note, on first invocation of the build script, a GHC is being installed for execution. However, if you build HIE for every GHC, no GHC is downloaded twice.
The GHC used for the `Shakefile.hs` can be adjusted in `shake.yaml` by using a different resolver.

Available commands can be seen with:

```bash
stack ./Shakefile.hs help
```

Remember, this will take time to download a Stackage-LTS and an appropriate GHC. However, afterwards all commands should work as expected. 

#### Install specific GHC Version with Shake

Install **Nightly** (and hoogle docs):

```bash
stack ./Shakefile.hs hie-8.6.3
stack ./Shakefile.hs build-doc-8.6.3
```

Install **LTS** (and hoogle docs):

```bash
stack ./Shakefile.hs hie-8.4.4
stack ./Shakefile.hs build-doc-8.4.4
```

#### Install *all* available GHC versions with Shake

*Warning*: Requires 20+ GB of space and potentially more than 2 hours to install, so please be patient!

This will:

* install all supported GHC versions (8.2.1 - 8.6.3)
* name them as expected by the VS Code plugin
* build local hoogle docs for each version

```bash
stack ./Shakefile.hs build-all
```

Then add

```json
"languageServerHaskell.useCustomHieWrapper": true,
"languageServerHaskell.useCustomHieWrapperPath": "hie-wrapper",
```

to VS Code user settings.

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

#### Using VS Code with Nix

`.config/nixpkgs/config.nix` sample:

``` nix
with import <nixpkgs> {};

let
  hie = (import (fetchFromGitHub {
                   owner="domenkozar";
                   repo="hie-nix";
                   rev="e3113da";
                   sha256="05rkzjvzywsg66iafm84xgjlkf27yfbagrdcb8sc9fd59hrzyiqk";
                 }) {}).hie84;
in
{
  allowUnfree = true;
  packageOverrides = pkgs: rec {

    vscode = pkgs.vscode.overrideDerivation (old: {
      postFixup = old.postFixup + ''
        wrapProgram $out/bin/code --prefix PATH : ${lib.makeBinPath [hie]}
      '';
    });

  };
}
```

### Using HIE with Sublime Text

* Make sure HIE is installed (see above) and that the directory stack put the `hie` binary in is in your path
  * (usually `~/.local/bin` on unix)
* Install [LSP](https://packagecontrol.io/packages/LSP) using [Package Control](https://packagecontrol.io/)
* From Sublime Text, press Command+Shift+P and search for Preferences: LSP Settings
* Paste in these settings. Make sure to change the command path to your `hie`

```
{
"clients": {
  "haskell-ide-engine": {
    "command": ["hie"],
    "scopes": ["source.haskell"],
    "syntaxes": ["Packages/Haskell/Haskell.sublime-syntax"],
    "languageId": "haskell",
  },
},
}
```

Now open a haskell project with Sublime Text. You should have these features available to you:

1. Errors are underlined in red
2. LSP: Show Diagnostics will show a list of hints and errors
3. LSP: Format Document will prettify the file

### Using HIE with Vim or Neovim

As above, make sure HIE is installed. These instructions are for using the [LanguageClient-neovim](https://github.com/autozimu/LanguageClient-neovim) client.

#### vim-plug
If you use [vim-plug](https://github.com/junegunn/vim-plug), then you can do this by e.g.
including the following line in the Plug section of your `init.vim`:

```
Plug 'autozimu/LanguageClient-neovim', {
    \ 'branch': 'next',
    \ 'do': './install.sh'
    \ }
```

and issuing a `:PlugInstall` command within neovim.

#### Vim 8.0
Clone [LanguageClient-neovim](https://github.com/autozimu/LanguageClient-neovim)
into `~/.vim/pack/XXX/start/`, where `XXX` is just a name for your "plugin suite".

#### Sample `~/.vimrc`

```vim
set rtp+=~/.vim/pack/XXX/start/LanguageClient-neovim
let g:LanguageClient_serverCommands = { 'haskell': ['hie-wrapper'] }
```

You'll probably want to add some mappings for common commands:

```vim
nnoremap <F5> :call LanguageClient_contextMenu()<CR>
map <Leader>lk :call LanguageClient#textDocument_hover()<CR>
map <Leader>lg :call LanguageClient#textDocument_definition()<CR>
map <Leader>lr :call LanguageClient#textDocument_rename()<CR>
map <Leader>lf :call LanguageClient#textDocument_formatting()<CR>
map <Leader>lb :call LanguageClient#textDocument_references()<CR>
map <Leader>la :call LanguageClient#textDocument_codeAction()<CR>
map <Leader>ls :call LanguageClient#textDocument_documentSymbol()<CR>
```

Use <kbd>Ctrl+x</kbd><kbd>Ctrl+o</kbd> (`<C-x><C-o>`) to open up the auto-complete menu,
or for asynchronous auto-completion, follow the setup instructions on
[LanguageClient](https://github.com/autozimu/LanguageClient-neovim).

If you'd like diagnostics to be highlighted, add a highlight group for `ALEError`/`ALEWarning`/`ALEInfo`,
or customize ` g:LanguageClient_diagnosticsDisplay`:

```vim
hi link ALEError Error
hi Warning term=underline cterm=underline ctermfg=Yellow gui=undercurl guisp=Gold
hi link ALEWarning Warning
hi link ALEInfo SpellCap
```

If you're finding that the server isn't starting at the correct project root,
it may also be helpful to also specify root markers:

```vim
let g:LanguageClient_rootMarkers = ['*.cabal', 'stack.yaml']
```

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

### Using HIE with Spacemacs on Nix Based Projects

If you use HIE with spacemacs on nix-built haskell projects, you may want to try
out [this spacemacs layer](https://github.com/benkolera/spacemacs-hie-nix). It
has installation instructions which includes a nix expression to install
everything that hie needs in your environment. It wraps the hie binary calls to
use nix-sandbox to find the closest ancestor directory that has nixfiles.

It is still pretty new and may change drastically as the author understands the
lsp, lsp-ui, lsp-haskell, hie stack a bit better. PRs and feedback are very
welcome on the layer's repo if you find it useful and/or lacking in some way.

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

Or you can set the environment variable `HIE_HOOGLE_DATABASE` to specify a specific database.

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


### cannot satisfy -package-id \<package\>

#### Is \<package\> base-x? 
Make sure that you are running the correct version of hie for your version of ghc, or check out hie-wrapper.

#### Is there a hash (#) after \<package\>?
Delete any `.ghc.environment*` files in your project root and try again. (At the time of writing, cabal new-style projects are not supported with ghc-mod)

#### Otherwise
Try running `cabal update`. 
