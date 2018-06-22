# Packages

```
+----------------------------+
|                            |
|     haskell-ide-engine     |
|                            |
| +------------------------+ |    +------------------+
| |                        | |    |                  |
| | haskell-ide-engine-api | +----> haskell-lsp-test |
| |                        | |    |                  |
| +------------------------+ |    +-+----------------+
|                            |      |
+--------------------+-------+      |
                     |              |
                     |              |
                +----v--------------v---+
                |                       |
                |      haskell-lsp      |
                |                       |
                | +-------------------+ |
                | |                   | |
                | | haskell-lsp-types | |
                | |                   | |
                | +-------------------+ |
                |                       |
                +-----------------------+

```

HIE is built upon various separate repositories and packages:

## [haskell-lsp](https://github.com/alanz/haskell-lsp)
Provides core functions for running a LSP server:
- Sending messages
- Parsing and receiving messages to various handlers
- Mirroring client documents in a virtual file system (VFS)

### haskell-lsp-types
A separate package within haskell-lsp that provides all the types as described in the specification.
Includes lenses and some helper functions for converting between types.
The separate package prevents unnecessary rebuilds of the template haskell files (which can take a long time) when working on haskell-ide-engine or haskell-lsp.

## [haskell-ide-engine](https://github.com/haskell/haskell-ide-engine)
An executable server that routes LSP requests to various tools in the haskell ecosystem.
Builds on top of haskell-lsp.
Uses plugins to connect the tools into the server.
Add plugin functionality in the `plugins` directory, and then hook it up to an appropriate LSP feature in `LspStdio.hs`.

### hie-plugin-api
Types, monads and functions for the plugins in `haskell-ide-engine`.

## haskell-lsp-test
A testing framework for LSP servers where scenarios can be captured or described in terms of messages.
Used by HIE for functional tests. 