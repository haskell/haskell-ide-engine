From https://www.reddit.com/r/haskell/comments/3z0ukd/reflecting_on_haskells_2015_and_my_wishes_for_2016/cyimsnp 

```
Off the top of my head, ordered approximately by importance:

    on-the-fly typechecking
    autocompletion
    documentation and type lookup; types also for compound expressions
    semi-automatic management of imports, dependencies and LANGUAGE pragmas
    auto-formatting
    jump-to-definition
    syntax highlighting that's actually correct and complete
    HLint-style suggestions
    profiling, testing and benchmarking integration

Many of these features are already covered by existing tools, but either with unsatisfactory
performance or not particularly well integrated. (No offense intended; I imagine that writing
Haskell tooling is exceptionally hard.)

If a majority of these features became a reality, that'd make me very happy indeed. Good luck
to you venerable fighters for a better Haskell experience.
```
From https://wiki.haskell.org/IDEs

This is a list of features that any Haskell IDE could or should have. Existing Haskell IDEs generally support some subset of these features.

- Syntax highlighting (e.g. for Haskell, Cabal, Literate Haskell, Core, etc.) 
- Macros (e.g. inserting imports/aligning/sorting imports, aligning up text, transposing/switching/moving things around) 
- Type information (e.g. type at point, info at point, type of expression) 
- IntelliSense/completion (e.g. jump-to-definition, who-calls, calls-who, search by type, completion, etc.) 
- Project management (e.g. understanding of Cabal, configuration/building/installing, package sandboxing) 
- Interactive REPL (e.g. GHCi/Hugs interaction, expression evaluation and such) 
- Knowledge of Haskell in the GHCi/GHC side (e.g. understanding error types, the REPL, REPL objects, object inspection) 
- Indentation support (e.g. tab cycle, simple back-forward indentation, whole area indentation, structured editing, etc.) 
- Proper syntactic awareness of Haskell (e.g. with a proper parser and proper editor transpositions a la the structured editors of the 80s and Isabel et al) 
- Documentation support (e.g. ability to call up documentation of symbol or module, either in the editor, or in the browser) 
- Debugger support (e.g. stepping, breakpoints, etc.) 
- Refactoring support (e.g. symbol renaming, hlint, etc.) 
- Templates (e.g. snippets, Zen Coding type stuff, filling in all the cases of a case, etc.) 
