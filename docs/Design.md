## This file is intended to capture the design as it evolves

At this point it is nothing but a sketch, to try and capture the things the may have been agreed so far.

It is likely to be wrong/off base, but is intended to provide a concrete point for discussion to revolve around.


### Plugin Architecture

For the initial phase, we will go for the simplest possible plugin model.
This should be something like the following

1. `haskell-ide-engine` is intended to be built and operated by a single version
   of GHC at any one time.

   What this means is that although it may support compilation with different
   versions of GHC, we will stick to only needing one at any given time, to
   avoid any issues around linking/operating.

   The intention is that this is the same version of GHC that the project
   being edited in the IDE is using.

   In the modern `stack` environment this may be a bit restrictive, but otherwise
   it introduces too much complexity in the beginning.

2. A plugin is provided as a library that can be installed from hackage/stack

   This allows a plugin developer to be able to work independently of `haskell-ide-backend`
   but still easily expose features to multiple IDEs.
   
   The simplest way of doing this will be to expose a lightweight "types-only" library
   which both the plugin and `haskell-ide-engine` will use to describe the features provided.
   
   This library is provisionally called `haskell-ide-plugin-api`. Bikeshedding welcome.
   
   A similar approach is being followed in https://github.com/neovimhaskell/nvim-hs, which can
   be used as a model.
   
3. There will be no dynamic loading of plugins

   A plugin will be something that `haskell-ide-engine` lists in its cabal file as a dependency.
   
   From a project perspective, we will have an open policy in terms of accepting PRs
   to add new plugins.
   
   This is another simplifying restriction, which may be lifted later when we have
   more insight, and running code.

### communication with the IDE

1. The eventual goal is that there is a clear separation between the types/messages
   transferred from `haskell-ide-engine` to the specific IDE and the transport layer
   used for this communication.
   
   In practice this means that some will communicate via JSON over HTTP, others using
   MSGPACK, and so on.
   
2. The operating environment will initially be assumed to be one instance of `haskell-ide-engine`
   talking to one IDE
   
   This will probably be valid for all time.
   
3. The messages exchanged need to be able to support the Idris mode rich interface

   To start with this may be simply having a message definition scheme that is
   sufficiently flexible, as this design goal is to some extent in conflict with
   the goal of getting something going as quickly as possible, which would suggest
   re-using existing code in current IDEs. This code would have to change quite a bit
   to support the richer functionality.
   
