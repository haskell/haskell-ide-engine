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

