# 0.8.0.0

- GHC 8.6.4 support added.
- Resolver bumped, LTS 13.10 for GHC 8.6.3, LTS 13.15 for GHC 8.6.4 (@alanz)
- Clarify install section of README.md (@antonlogvinenko)
- Clarify the spacemacs installation (@chkl)
- Further install.hs improvements
  - idempotent builds (@fendor)
  - Shake is now the only supported method of building HIE,
    remove no longer needed Makefile and build-all.ps1 (@Anrock)
  - only generate the hoogle database once (@fendor)
  - install hoogle if not found (@fendor)
- Add support for pattern synonyms in ghc-mod plugin (@anton-dessiatov)
- prevent hie crash if hlint crashes (@fendor)

# 0.7.0.0

- Resolver bumped, LTS 13.9 for GHC 8.6.3 (@alanz)
- Ongoing improvements of `install.hs` installation process and
  documentation. (@fendor, @power-fungus, @Anrock, @Hogeyama )
  - Improved documentation
  - can now also build via `cabal new-build`
  - improved cross-platform support
- Introduce [floskell](https://github.com/ennocramer/floskell) as an
  alternative formatting provider (@bubba, @AlexeyRaga, @luigy)
  - Introduces `formattingProvider` as a plugin API function.
  - Can be selected via configuration option `formattingProvider`
- Respects the `only` parameter of codeAction requests (@bubba)
  - So can request only `quickfix` or `refactor` code actions.
- Bump hlint to 2.1.15 (@alanz)

# 0.6.0.0

- Resolver bumped, LTS 13.5 for GHC 8.6.3 (@alanz)
- Use internal library hie-test-utils for testing (@bubba)
- Read files in UTF8 mode in ghc-mod (@alanz)
- documentation updates
  - document reactorPidcache (@bubba)
  - Add a note in README about dyld path for macOS builds (@kubum)
  - document workaround for missing gmp library (@Rhywun)
  - Change --recursive to --recurse-submodules when cloning
    (@leifmetcalf)
- Speed up CI on circleci (@bubba)
- Build via make
  - Recursively sync and update submodules in Makefile (@bubba)
- build via shake
  - Add 8.4.2 and 8.2.1 HIE versions to Shakefile (@Anrock)
  - Sync & update submodules recursively in Shakefile (@Anrock)
  - Remove v1 prefix from cabal commands in Shakefile (@Anrock)
  - Rename Shakefile.hs to install.hs (@Anrock)
  - install.hs: Sync submodules and install cabal before building
    `dist`(@fendor)
  - Display error message on stack-compilation errors (@power-fungus)
    Suggests doing `stack clean` and trying again.
  - Generate Shake help message based on GHC version (@fendor)
- remove EKG to reduce dependency footprint (@bubba)
- Bump hlint to 2.1.14 (@alanz)
  (for GHC versions from 8.2.2 to 8.6.3)

And there is work happening currently on a new implementation of
`cabal-helper` to fully support `cabal new-build`, together with a
rework of `ghc-mod-core` to make use of the new `cabal-helper`. This
is a complex effort, and will take some time, but is being tackled by
@DanielG, assisted by @fendor and @power-fungus,


# 0.5.0.0

 - Introduce Shakefile as build alternative (@fendor)
 - Support GHC 8.6.3
 - Stability improvements
   - fixed process dying on some code action requests (#1018)
   - Deal with missing system GHC in hie-wrapper (#1012)
   - Deal with missing system GHC in hie
   - Return an error diagnostic when a project cannot be built (#1011)
 - Completion now strips out OccName prefixes added by GHC (#996)
 - improve building on windows
 - improve building on macos
 - Run diagnostics on file save. It used to only do it on change.

# 0.4.0.1

- Install cabal / Cabal (needed for Cabal 2.4.0.1 support) via stack,
  for when there is no other GHC installed.

# 0.4.0.0

- Supports GHC 8.6
- Preliminary support for cabal new-build projects
- Can install via cabal new-build
- Completions: more comprehensive filtering of name prefixes
  introduced by GHC
- Replace bat script with PowerShell, update Windows instructions in
  README (@fsoikin)

# 0.3.0.0

 - LSP mode is now the default, and the `--lsp` flag has no effect
   - The `--json` flag can be used for JSON transport
 - HIE now warns you if there is mismatch between the HIE GHC verison and the project GHC version
 - Add Liquid Haskell support
 - Add support for hierarchical document symbols
 - Add many new types of code actions
   - Typed holes
   - HaRe refactoring
   - Misspelled variables
   - Missing top-level signatures
   - Prefix unused terms with `_`
   - Case splitting
   - Suggested pragmas and language extensions
 - The parsed output from a module is now cached
   - Some features are now available without the need for the module to typecheck first
 - Improve code completion
   - Suggests modules that can be imported
   - Suggests GHC extensions
   - Recognizes when completing a type or expression
   - Provides snippets for arguments to functions
 - Add the ability to set an explicit hoogle database

Thanks to the contributors for this release:
 - @Avi-D-coder
 - @Gurkenglas
 - @Technix
 - @alanz
 - @apeyroux
 - @bbarker
 - @bubba
 - @cblp
 - @cronokirby
 - @expipiplus1
 - @jhrcek
 - @jkachmar
 - @lorenzo
 - @m13m
 - @meck
 - @mpilgrem
 - @waddlaw

# 0.2.2.0

Add more code actions for various diagnostics:
 - Add missing imports and remove redundant ones via HsImport
 - Add missing packages to .cabal or package.yaml files
 - Correct typos suggested by GHC

# 0.2.1.0

Include case split command, from @txsmith

# 0.2.0.0

