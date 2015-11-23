# Challenges encountered by existing tooling

### ghc-mod

 1. Linking aginst Cabal directly meant lots of breakage when interacting with the on disk configuration state. (Since solved by using wrapper)
 2. Supporting ill defined interfaces and protocols is hard to impossible. Over the course of it's almost 100 releases (!!!) compatibility was broken way too often.
 3. Supporting many GHC versions simultaniously is very hard since they keep breaking the API.
 4. Linking against GHC means simmilar problems as with linking against Cabal, i.e. when the user upgrades their GHC binary stuff will break.

:memo: Don't link against Cabal directly ever
:memo: (maybe) Target only one GHC version at a time or provide some compatibility layer
:memo: Let's get the interfaces mostly right on the first go
:memo: Handle changing compiler versions transparently

### ide-backend / stack-ide

 1.  For various reasons mainly related to its original usecase, all of the information yielded by compilation gets sent to a separate process than ide-backend-server (ghc). While it's nice to have a datatype for all the info yielded by a compilation, this seems wasteful from a performance perspective. It also means extending ide-backend with new features requires a lot more boilerplate than it ought to.

 2. Another one of the reasons to have this multiple process architecture was to be able to accept mutations of the configuration of GHC (changing flags, etc), and intelligently either get GHC to update its flags or restart the ide-backend-server.  This turned out to be very tricky, and we had lots of different issues and it's still imperfect.  

:memo: Let's just force a backend restart if you change ghc or RTS options.

 3. With stack-ide, we ended up wrapping the pure API with another layer of datatypes for conversion to JSON.  This meant the dataflow from the backend looked like: GHC types -> internal datatypes with explicit sharing -> byte serialized representation sent to ide-backend user -> public datatypes with sharing reified -> ide-backend function call -> stack-ide json datatype

:memo: Lets avoid a lot of layers.

## Flycheck

 1. Works pretty well/reliably (for @bitemyapp anyway), but a bit slow. Basically fires off a `stack build` (or `cabal build`) or `hlint` from the command line and parses stdout.
 2. Emacs-only
 3. Easier to get working reliably than `ghc-mod`
