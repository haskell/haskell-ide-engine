# Challenges encountered by existing tooling

### ghc-mod

 1. Relying on files/binary data that Cabal uses which were never meant to be part of an API or protocol meant breakage was frequent and needing a specific version of ghc-mod to be matched with a specific version of Cabal.
 2. A bit slow, especially on larger projects.
 3. Related to `1.`, hard for new people to get installed and working with their editor + projects.

:memo: Don't rely on files/binary data that Cabal uses
:memo: It must be fast to be pleasant to use

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
