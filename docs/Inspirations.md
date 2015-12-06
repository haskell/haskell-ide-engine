## A list of links to concepts which may be worth looking at

Basically a list of what other projects are doing to solve similar problems, in no particular order

* https://discuss.atom.io/t/whats-the-difference-between-packages-and-services/15538/3 atom service description, similar in concept to the (currently unused) `Service` layer in the plugin api.

* https://github.com/nominolo/scion-tools and https://github.com/nominolo/scion - See below

* https://github.com/kRITZCREEK/psc-ide - Editor Support for the PureScript programming language

* https://github.com/atom-haskell/ide-haskell - atom plugin
 
* https://github.com/the-lambda-church/merlin - Context sensitive completion for OCaml in Vim and Emacs

* https://github.com/Sarcasm/irony-mode - A C/C++ minor mode for Emacs powered by libclang

* https://github.com/epost/psc-query - PureScript source code knowledge extraction and querying support.

* https://github.com/neovim/neovim/wiki/Plugin-UI-architecture

* https://github.com/neovimhaskell/nvim-hs - Neovim API for Haskell plugins as well as the plugin provider 

* https://github.com/ensime/ensime-server - ENhanced Scala Interaction Mode for text Editors

* https://github.com/knupfer/haskell-emacs - to check out communication between Emacs and Haskell

* https://github.com/omnisharp - Cross platform *** development in the editor of your choice
  (ignore the language, look at the tooling). Detail: https://github.com/OmniSharp/omnisharp-emacs/blob/master/src/actions/omnisharp-auto-complete-actions.el

* https://downloads.haskell.org/~ghc/latest/docs/html/libraries/ghc-7.10.2/src/Hooks.html#Hooks
  GHC compiler hooks, can be used to provide information available in the given
  phase, and return it to ghc-mod, as used in ide-backend:
  https://github.com/fpco/ide-backend/blob/master/ide-backend-server/Server.hs#L180

* Reddit thread: https://www.reddit.com/r/programming/comments/3o4o59/standard_interface_between_a_text_editor_and_an/

* http://company-mode.github.io/ - pluggable completion framework for emacs

* https://github.com/idris-hackers/idris-mode - Idris language support is really good

* http://andrew.gibiansky.com/blog/ipython/ipython-kernels/ - IPython architecture description

* https://github.com/gibiansky/IHaskell

* https://www.rust-lang.org/ides.html Rust Language proposed IDE support

* [emacs-devel mailing list: re IDE](https://lists.gnu.org/archive/html/emacs-devel/2015-10/msg00669.html)

* [Incremental lexer for IDE](http://blog.haskell-exists.com/yuras/posts/incremental-lexer.html)

* [Lazy Functional Incremental Parsing](http://www.cse.chalmers.se/~bernardy/FunctionalIncrementalParsing.pdf) by JP Bernardy, as used in Yi editor

* [Robust & Precise incremental parsing of Haskell](http://publications.lib.chalmers.se/records/fulltext/117337.pdf) thesis by Anders Karlsson, also wrt Yi editor


## Scion-Tools

From the `haskell-ide` mailing list:

```
Yes, that is the concept for Scion (v2). I stopped working on it at
some point because it was so messy to get the necessary information
out of GHC. I did a little GHC API proof of concept with GHC 7.10
recently. That looks more promising. Also, Stack could take care of a
lot of build issues.

The basic idea is:
 - Building is not managed by GHC's --make mode. Instead an external
build manager (e.g., based on Shake) invokes compiler in one-shot
mode.
 - The compiler is basically GHC + hooks.
 - The compiler generates .hi files, optionally .o files, and .meta
files. This contains all meta data which may grow over time: error
messages, parse tree, renamed/typechecked code

A (logically) separate service manages and caches additional
information derived from the meta data. For example,
 - all error messages in the whole project (or which ones might be
outdated because a dependency has changed)
 - definition sites of all identifiers in the project
 - use sites of things
 - etc.

The IDE layer would sit on top of this separate service.

Open issues:
 - How are projects specified? Ideally stack based.
 - How are custom build steps integrated?
 - Will this work for GHCi?

I talked about this with Duncan Coutts (who was involved in the
backend that FP Complete IDE work) and they came to basically the same
conclusion. This work has now been open sourced, but I only took a
brief look at it a while ago. Probably worth another closer look.
```
