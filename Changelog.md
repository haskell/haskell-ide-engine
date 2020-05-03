## 1.4

NOTE: haskell-ide-engine is in transition, the new home for it will be
[haskell-language-server](https://github.com/haskell/haskell-language-server),
which does not currently have feature parity with `hie`.
Once that hurdle is crossed, the main emphasis will be there, and
`hie` will eventually be deprecated.

## In this version

- cabal to 2020-05-02T10:11:15Z
- stack-8.8.3 to lts-15.10
- stack to nightly-2020-05-01

## Changes

- Bump resolvers
([#1757](https://github.com/haskell/haskell-ide-engine/pull/1757) by @alanz)
- Move finding the package for a filepath
([#1750](https://github.com/haskell/haskell-ide-engine/pull/1750) by @fendor)
- Update Sublime Text HIE command
([#1742](https://github.com/haskell/haskell-ide-engine/pull/1742) by @ssanj)
- Use installed ghc in stack linux builds
([#1737](https://github.com/haskell/haskell-ide-engine/pull/1737) by @jneira)
- Strip RTS flags, since we cant honor them
([#1736](https://github.com/haskell/haskell-ide-engine/pull/1736) by @fendor)
- Support for ghc-8.8.3
([#1697](https://github.com/haskell/haskell-ide-engine/pull/1697) by @jneira)
- Haddock source file names may use either dot or dash as separator
([#1723](https://github.com/haskell/haskell-ide-engine/pull/1723) by @wz1000)

## 1.3

NOTE: haskell-ide-engine is in transition, the new home for it will be
[haskell-language-server](https://github.com/haskell/haskell-language-server),
which does not currently have feature parity with `hie`. In particular
it does not support multi-cradles, so can only open a single component
of a project, as configured into its `hie.yaml` file.

Once that hurdle is crossed, the main emphasis will be there, and
`hie` will eventually be deprecated.

## In this version

- cabal now index state 2020-04-06T20:27:36Z
- GHC 8.8.2 is lts-15.3

## Changes

- Make install script use unix-time-0.4.7
([#1715](https://github.com/haskell/haskell-ide-engine/pull/1715) by @jneira)
- HsImport: Return codeactions importing from Internal modules last
([#1703](https://github.com/haskell/haskell-ide-engine/pull/1703) by @expipiplus1)
- Extend FormatSpec with a data types
([#1605](https://github.com/haskell/haskell-ide-engine/pull/1605) by @EncodePanda)
- Update hie-plugin-api to use unliftio-core 0.2.0.1
([#1698](https://github.com/haskell/haskell-ide-engine/pull/1698) by @fendor)
- Use sha instead revision for hsimport and other fixes
([#1694](https://github.com/haskell/haskell-ide-engine/pull/1694) by @jneira)
- Fix: haddock for hie-plugin-api
([#1696](https://github.com/haskell/haskell-ide-engine/pull/1696) by @fendor)
- Fix #1578 - Redesign option parsing for main executable
([#1671](https://github.com/haskell/haskell-ide-engine/pull/1671) by @gdziadkiewicz)
- Report progress as a percentage
([#1692](https://github.com/haskell/haskell-ide-engine/pull/1692) by @expipiplus1)
- Correct pull request url and formatting in the changelog
([#1684](https://github.com/haskell/haskell-ide-engine/pull/1684) by @jneira)
- Fix typos in README. Add hint to speed up HIE compilation
([#1685](https://github.com/haskell/haskell-ide-engine/pull/1685) by @mb720)


## 1.2

NOTE: haskell-ide-engine is in transition, the new home for it will be
[haskell-language-server](https://github.com/haskell/haskell-language-server),
which does not currently have feature parity with `hie`. In particular
it does not support multi-cradles, so can only open a single component
of a project, as configured into its `hie.yaml` file.

Once that hurdle is crossed, the main emphasis will be there, and
`hie` will eventually be deprecated.

## In this version

- cabal now index state 2020-03-03T21:13:56Z
- GHC 8.8.2 is lts-15.2
- GHC 8.6.5 is lts-14.22
- hlint is 2.2.11
- brittany is 0.12.1.1
- hoogle is 5.0.17.15
- haskell-src-exts is 1.22.0

## Changes

- Use haskell-src-exts 1.22
([#1679](https://github.com/haskell/haskell-ide-engine/pull/1679), by @jneira)
- Bump resolvers and hlint to 2.2.11
([#1678](https://github.com/haskell/haskell-ide-engine/pull/1678), by @alanz)
- Improvements of install script and macos-installhs-cabal new azure job
([#1665](https://github.com/haskell/haskell-ide-engine/pull/1665), by @jneira)
- Azure: try to generate hoogle twice to avoid 403 http errors for macos
([#1662](https://github.com/haskell/haskell-ide-engine/pull/1662), by @jneira)
- Enable some more tests in windows
([#1659](https://github.com/haskell/haskell-ide-engine/pull/1659), by @jneira)
- Add azure badge to README
([#1661](https://github.com/haskell/haskell-ide-engine/pull/1661), by @jneira)
- Azure: not trigger pr builds by path
([#1633](https://github.com/haskell/haskell-ide-engine/pull/1633), by @jneira)
- Fix failing tests in windows (reloaded)
([#1655](https://github.com/haskell/haskell-ide-engine/pull/1655), by @jneira)
- Replace one more haskell-ide ocurrence
([#1652](https://github.com/haskell/haskell-ide-engine/pull/1652), by @jneira)
- Set current working directory when executing project ghc
([#1654](https://github.com/haskell/haskell-ide-engine/pull/1654), by @fendor)
- Use haskell-lsp-0.20
([#1645](https://github.com/haskell/haskell-ide-engine/pull/1645), by @jneira)
- Remove Comma
([#1642](https://github.com/haskell/haskell-ide-engine/pull/1642), by @nilsmartel)


# 1.1

NOTE: haskell-ide-engine is in transition, the new home for it will be
[haskell-language-server](https://github.com/haskell/haskell-language-server),
which does not currently have feature parity with `hie`. In particular
it does not support multi-cradles, so can only open a single component
of a project, as configured into its `hie.yaml` file.

Once that hurdle is crossed, the main emphasis will be there, and
`hie` will eventually be deprecated.

## In this version

- cabal now index state 2020-01-31T21:11:24Z
- GHC 8.8.2 is nightly-2020-01-31
- GHC 8.6.5 is lts-14.22
- hlint is 2.2.10
- brittany is 0.12.1.1

## Changes

- Install script with cabal: check there is one ghc in $PATH
([#1632](https://github.com/haskell/haskell-ide-engine/pull/1632), by @jneira)
- Update README.md
([#1636](https://github.com/haskell/haskell-ide-engine/pull/1636), by @flip111)
- Azure: not trigger pr builds by path
([#1633](https://github.com/haskell/haskell-ide-engine/pull/1633), by @jneira)
- Add dev target to stack install.hs
([#1615](https://github.com/haskell/haskell-ide-engine/pull/1615), by @jneira)
- Fix cabal-hie-install in windows azure ci
([#1627](https://github.com/haskell/haskell-ide-engine/pull/1627), by @jneira)
- Replace one more haskell-ide in README
([#1630](https://github.com/haskell/haskell-ide-engine/pull/1630), by @jneira)
- Bump resolvers, hlint, brittany
([#1622](https://github.com/haskell/haskell-ide-engine/pull/1622), by @alanz)
- Update HIE to use latest hie-bios
([#1601](https://github.com/haskell/haskell-ide-engine/pull/1601), by @fendor)
- Add instructions for installing HIE + GHC as a VS Code Devcontainer
([#1624](https://github.com/haskell/haskell-ide-engine/pull/1624), by @GavinRay97)
- Readme: haskell-ide -> haskell-language-server
([#1625](https://github.com/haskell/haskell-ide-engine/pull/1625), by @andys8)
- Deduplicate main for hie/hie-wrapper
([#1610](https://github.com/haskell/haskell-ide-engine/pull/1610), by @Gurkenglas)
- Azure fix win cabal 8.4.4
([#1619](https://github.com/haskell/haskell-ide-engine/pull/1619), by @jneira)
- Mention the new haskell-ide in the README
([#1612](https://github.com/haskell/haskell-ide-engine/pull/1612), by @jneira)
- Ormolu range format support
([#1602](https://github.com/haskell/haskell-ide-engine/pull/1602), by @Avi-D-coder)
- Add 8.8.2 stack file and CI
([#1607](https://github.com/haskell/haskell-ide-engine/pull/1607), by @bubba)
- Azure update macos and try to fix windows+stack builds
([#1609](https://github.com/haskell/haskell-ide-engine/pull/1609), by @jneira)
- Update required stack version
([#1603](https://github.com/haskell/haskell-ide-engine/pull/1603), by @wataru86)
- Remove compiler warnings
([#1600](https://github.com/haskell/haskell-ide-engine/pull/1600), by @EncodePanda)
- Remove hlint.yaml from azure releases and readme
([#1598](https://github.com/haskell/haskell-ide-engine/pull/1598), by @jneira)
- Fix error message parsing to import types
([#1597](https://github.com/haskell/haskell-ide-engine/pull/1597), by @fendor)
- Pass Ormolu cradle flags & default-extensions
([#1589](https://github.com/haskell/haskell-ide-engine/pull/1589), by @Avi-D-coder)
- Fix multi source directories
([#1577](https://github.com/haskell/haskell-ide-engine/pull/1577), by @fendor)
- azure: add windows+cabal job and other improvements
([#1595](https://github.com/haskell/haskell-ide-engine/pull/1595), by @jneira)
- Allow newer optparse-applicative for ormolu (stack version)
([#1586](https://github.com/haskell/haskell-ide-engine/pull/1586), by @jneira)
- Azure builds improvements
([#1584](https://github.com/haskell/haskell-ide-engine/pull/1584), by @jneira)
- Drop GHC version 8.6.1, 8.6.2 and 8.6.3 (#1592)
([#1594](https://github.com/haskell/haskell-ide-engine/pull/1594), by @fendor)
- Demote HsImport func-tests to unit-test
([#1591](https://github.com/haskell/haskell-ide-engine/pull/1591), by @fendor)
- Update hlint to 2.2.8 and ormolu to 0.0.3
([#1588](https://github.com/haskell/haskell-ide-engine/pull/1588), by @alanz)
- Allow newer optparse-applicative for ormolu
([#1583](https://github.com/haskell/haskell-ide-engine/pull/1583), by @bubba)
- Make cabal-hie-install executable
([#1581](https://github.com/haskell/haskell-ide-engine/pull/1581), by @maoe)
- Ormolu formatter support
([#1481](https://github.com/haskell/haskell-ide-engine/pull/1481), by @DavSanchez)
- GHC 8.8 support
([#1482](https://github.com/haskell/haskell-ide-engine/pull/1482), by @Avi-D-coder)
- Load all possible haskell source files
([#1569](https://github.com/haskell/haskell-ide-engine/pull/1569), by @fendor)
- Enable all working test suites and add linux-cabal job in Azure
([#1571](https://github.com/haskell/haskell-ide-engine/pull/1571), by @jneira)
- Mention hlint data file handling in readme
([#1573](https://github.com/haskell/haskell-ide-engine/pull/1573), by @jneira)
- Enable azure releases and some fixes
([#1545](https://github.com/haskell/haskell-ide-engine/pull/1545), by @jneira)
- Add cabal freeze files and use them automatically
([#1561](https://github.com/haskell/haskell-ide-engine/pull/1561), by @hasufell)
- Add unit-tests for Cabal-Helper cradles
([#1552](https://github.com/haskell/haskell-ide-engine/pull/1552), by @fendor)
- Don't mix stack with cabal
([#1557](https://github.com/haskell/haskell-ide-engine/pull/1557), by @hasufell)
- Avoid building HIE twice
([#1562](https://github.com/haskell/haskell-ide-engine/pull/1562), by @hasufell)
- Readme lsp flag
([#1559](https://github.com/haskell/haskell-ide-engine/pull/1559), by @Anrock)
- Fix haddock documentation for Cradle.hs
([#1549](https://github.com/haskell/haskell-ide-engine/pull/1549), by @fendor)


# 1.0.0.0

- NOTE: 1.0 status does **not** mean it is now stable.
- This is a major version bump, because the internal architecture has
  changed to make use of the new
  [hie-bios](https://github.com/mpickering/hie-bios) from @mpickering
  (and a host of others).

Some of the implications of this are

- Cabal 3.0 support.
- Support for stack scripts.
- Work on test, executable, benchmark and library components at the same time.
- You can set the build-tool (stack or cabal) explicitly.
- Various memory leaks have been fixed.
- Various performance improvements.

Also, until `hie-bios` provides a means to get a full module graph for
the project, we haved disabled HaRe (rename, caseSplitCmd, etc...).
It is better to not have a tool, than one that sort-of works, and we
did not want to hold back all the other work that has been done.


## Changes

- Fallback to direct cradle if no project context can be found
([#1551](https://github.com/haskell/haskell-ide-engine/pull/1551), by @fendor)

- Bump resolvers `lts-14.20` for GHC 8.6.5.
([#1547](https://github.com/haskell/haskell-ide-engine/pull/1547), by @alanz)

- Improve quality and information density of error message
([#1522](https://github.com/haskell/haskell-ide-engine/pull/1522), by @fendor)

- Add cmd script to install with cabal
([#1542](https://github.com/haskell/haskell-ide-engine/pull/1542), by @jneira)

- Tweak hie-wrapper and hie exe startup messages
([#1539](https://github.com/haskell/haskell-ide-engine/pull/1539), by @alanz)

- First pass at printing out debug info if started without --lsp
([#1538](https://github.com/haskell/haskell-ide-engine/pull/1538), by @alanz)

- Return hlint code actions as type 'quickfix'
([#1537](https://github.com/haskell/haskell-ide-engine/pull/1537), by @alanz)

- Readme: Remove non-existing troubleshooting entry
([#1534](https://github.com/haskell/haskell-ide-engine/pull/1534), by @andys8)

- Add sample hie.yaml files for stack and cabal
([#1533](https://github.com/haskell/haskell-ide-engine/pull/1533), by @alanz)

- Look for stack.yaml before selecting .cabal location as project root
([#1531](https://github.com/haskell/haskell-ide-engine/pull/1531), by @fendor)

- Change Maybe LspFuncs to LspFuncs inside IdeEnv
([#1523](https://github.com/haskell/haskell-ide-engine/pull/1523), by @bubba)

- Replaced cabal-helper submodule with hackage version
([#1521](https://github.com/haskell/haskell-ide-engine/pull/1521), by @alanz)

- Adapt GhcModPluginSpec after merge of #1496
([#1507](https://github.com/haskell/haskell-ide-engine/pull/1507), by @alanz)

- Find the libdir directory of ghc at run-time
([#1496](https://github.com/haskell/haskell-ide-engine/pull/1496), by @fendor)

- Restore the ghcmod plugin command routing
([#1505](https://github.com/haskell/haskell-ide-engine/pull/1505), by @alanz)

- Make errorm use errorM instead of warningM
([#1502](https://github.com/haskell/haskell-ide-engine/pull/1502), by @DavSanchez)

- Refactor plugins and commands now that JSON transport is gone
([#1492](https://github.com/haskell/haskell-ide-engine/pull/1492), by @bubba)

- Retain the --lsp option, to not break existing clients
([#1494](https://github.com/haskell/haskell-ide-engine/pull/1494), by @alanz)
updated 14 days ago

- Remove JSON transport
([#1489](https://github.com/haskell/haskell-ide-engine/pull/1489), by @bubba)

- Remove ghc-project-types
([#1487](https://github.com/haskell/haskell-ide-engine/pull/1487), by @alanz)

- Switch back to @DanielG cabal-helper
([#1488](https://github.com/haskell/haskell-ide-engine/pull/1488), by @bubba)

- Update Ubuntu dependency
([#1485](https://github.com/haskell/haskell-ide-engine/pull/1485), by @leifmetcalf)

- Some tidying up for tests
([#1483](https://github.com/haskell/haskell-ide-engine/pull/1483), by @bubba)

- Disable travis for now
([#1484](https://github.com/haskell/haskell-ide-engine/pull/1484), by @bubba)

- Implement the HIE Bios
([#1126](https://github.com/haskell/haskell-ide-engine/pull/1126), by @mpickering)

- Add mac build
([#1479](https://github.com/haskell/haskell-ide-engine/pull/1479), by @liam-ly)

- Unit test over the liquid haskell supported version
([#1449](https://github.com/haskell/haskell-ide-engine/pull/1449), by @jneira)


# 0.14.0.0

- Bump resolvers and deps `lts-14.16` for GHC 8.6.5.

  Key deps updated
  - floskell-0.10.2
  - hlint-2.2.4

- Hack awful script for generating changelog
([#1423](https://github.com/haskell/haskell-ide-engine/pull/1423), by @bubba)
- Update README.md
([#1472](https://github.com/haskell/haskell-ide-engine/pull/1472), by @Shenmin-Z)
- Fix typos; should be non-semantic
([#1469](https://github.com/haskell/haskell-ide-engine/pull/1469), by @bwignall)
- Update the instructions for Spacemacs
([#1466](https://github.com/haskell/haskell-ide-engine/pull/1466), by @sei40kr)
- Relax bounds of shake and use latest hie resolver
([#1464](https://github.com/haskell/haskell-ide-engine/pull/1464), by @jneira)
- Update README.md
([#1459](https://github.com/haskell/haskell-ide-engine/pull/1459), by @flip111)
- Various improvements and fixes over shake install
([#1452](https://github.com/haskell/haskell-ide-engine/pull/1452), by @jneira)
- Update LSP options for new haskell-lsp-0.18
([#1461](https://github.com/haskell/haskell-ide-engine/pull/1461), by @bubba)
- README.md > vim/coc setup > last comma in the json object doesn't parse.
([#1463](https://github.com/haskell/haskell-ide-engine/pull/1463), by @bChiquet)
- Update README.md
([#1458](https://github.com/haskell/haskell-ide-engine/pull/1458), by @flip111)
- Bump haskell-lsp et al to 0.18.0.0
([#1457](https://github.com/haskell/haskell-ide-engine/pull/1457), by @alanz)
- Unit test over the liquid haskell supported version
([#1449](https://github.com/haskell/haskell-ide-engine/pull/1449), by @jneira)
- Adapt liquid haskell tests for version 0.8.6.2
([#1440](https://github.com/haskell/haskell-ide-engine/pull/1440), by @alanz)
- Ignore the PATH fix if it is not set
([#1437](https://github.com/haskell/haskell-ide-engine/pull/1437), by @jneira)
- Make sure the liquid haskell test files are generated for unit-test
([#1438](https://github.com/haskell/haskell-ide-engine/pull/1438), by @alanz)
- Drop stack support for GHC 8.2.2
([#1430](https://github.com/haskell/haskell-ide-engine/pull/1430), by @fendor)
- Register rename and implementation provider
([#1427](https://github.com/haskell/haskell-ide-engine/pull/1427), by @bubba)
- Fix snippets configuration option not working
([#1424](https://github.com/haskell/haskell-ide-engine/pull/1424), by @bubba)


# 0.13.0.0

- Bump resolvers and deps `lts-14.11` for GHC 8.6.5, and
`nightly-2019-09-21` for nightly build, the last one to support GHC
8.6.5.

  Key deps updated
  - brittany-0.12.1
  - floskell-0.10.1
  - hlint-2.2.3
  - hsimport-0.11.0

  ([#1419](https://github.com/haskell/haskell-ide-engine/pull/1419), by @alanz)

- Update haskell-lsp to 0.17
([#1418](https://github.com/haskell/haskell-ide-engine/pull/1418), by @alanz)

- Add instructions about install cabal with stack in the README
([#1414](https://github.com/haskell/haskell-ide-engine/pull/1414), by @jneira)

- Robust tests
([#1413](https://github.com/haskell/haskell-ide-engine/pull/1413), by @alanz)

- Find and run cabal in user original $PATH
([#1406](https://github.com/haskell/haskell-ide-engine/pull/1406), by @jneira)

- Add stack-install-cabal target and confirmation messages
([#1405](https://github.com/haskell/haskell-ide-engine/pull/1405), by @jneira)

- Haskell lsp 0.16
([#1402](https://github.com/haskell/haskell-ide-engine/pull/1402), by @alanz)

- Handling Windows specific delimiters in func tests
([#1400](https://github.com/haskell/haskell-ide-engine/pull/1400), by @jneira)

- Fix more code actions in windows
([#1399](https://github.com/haskell/haskell-ide-engine/pull/1399), by @jneira)

- Upgrade network to 3.0.1.1
([#1395](https://github.com/haskell/haskell-ide-engine/pull/1395), by @jneira)

- Use the new key format in one line for azure cache
([#1394](https://github.com/haskell/haskell-ide-engine/pull/1394), by @jneira)

- Fix code renaming in windows
([#1392](https://github.com/haskell/haskell-ide-engine/pull/1392), by @jneira)

- Add CodeTriage badge
([#1381](https://github.com/haskell/haskell-ide-engine/pull/1381), by @NickSeagull)

- Add support for building with cabal-3.0.0.0
([#1379](https://github.com/haskell/haskell-ide-engine/pull/1379), by @jneira)

- Refactor backtick aware completion
([#1377](https://github.com/haskell/haskell-ide-engine/pull/1377), by @fendor)

- Add different Contexts for Module, import etc...
([#1375](https://github.com/haskell/haskell-ide-engine/pull/1375), by @fendor)

- Do not traverse into Generated bindings when creating TypeMap
([#1372](https://github.com/haskell/haskell-ide-engine/pull/1372), by @fendor)

- Readme: Mention cabal configure and restarting HIE for troubleshooting
([#1370](https://github.com/haskell/haskell-ide-engine/pull/1370), by @Infinisil)

- Split out completion from HieExtras
([#1369](https://github.com/haskell/haskell-ide-engine/pull/1369), by @bubba)

- Remove cabal check from stack builds
([#1368](https://github.com/haskell/haskell-ide-engine/pull/1368), by @ollef)

- Recommend Coc over LanguageClient-neovim
([#1367](https://github.com/haskell/haskell-ide-engine/pull/1367), by @Avi-D-coder)

- Install: Fix broken stack-build target and fix cabal run help msg
([#1363](https://github.com/haskell/haskell-ide-engine/pull/1363), by @fendor)

- Fix error message if outdated cabal dependency
([#1361](https://github.com/haskell/haskell-ide-engine/pull/1361), by @fendor)

- Made hlint dependency properly depend on version of ghc.
([#1355](https://github.com/haskell/haskell-ide-engine/pull/1355), by @LinuxUser404)

# 0.12.0.0

- Monthly resolver bump, `lts-13.30` for GHC 8.6.5, and `nightly-2019-07-31` for
  nightly build, rest are unchanged.
([#1352](https://github.com/haskell/haskell-ide-engine/pull/1352), by @alanz)

- Log OS in hie-wrapper
([#1351](https://github.com/haskell/haskell-ide-engine/pull/1351), by @chrismwendt)

- Fix completionItem/resolve sending snippets as plain text
([#1349](https://github.com/haskell/haskell-ide-engine/pull/1349), by @Avi-D-coder)

- Bump hlint to 2.2.2
([#1347](https://github.com/haskell/haskell-ide-engine/pull/1347), by @alanz)

- fail installation if cabal-version is too low
([#1344](https://github.com/haskell/haskell-ide-engine/pull/1344), by @power-fungus)

- Update vim links in TOC
([#1341](https://github.com/haskell/haskell-ide-engine/pull/1341), by @adamse)

- lift required stack-version to 2.1.1
([#1338](https://github.com/haskell/haskell-ide-engine/pull/1338), by @power-fungus)

- Replace every occurrence of build-doc by build-data
([#1336](https://github.com/haskell/haskell-ide-engine/pull/1336), by @w1gz)

- Use preview of pipeline caching in azure builds
([#1335](https://github.com/haskell/haskell-ide-engine/pull/1335), by @jneira)

- Simpler completion
([#1334](https://github.com/haskell/haskell-ide-engine/pull/1334), by @wz1000)

- Clarify README.md for Vim users
([#1331](https://github.com/haskell/haskell-ide-engine/pull/1331), by @mb720)

- Fix typos in Challenges.md
([#1329](https://github.com/haskell/haskell-ide-engine/pull/1329), by @mb720)

- Remove unnecessary extra-dep unix-time
([#1326](https://github.com/haskell/haskell-ide-engine/pull/1326), by @jneira)

- Use hlint-2.2
([#1325](https://github.com/haskell/haskell-ide-engine/pull/1325), by @alanz)

- Remove "hybrid" test cases
([#1324](https://github.com/haskell/haskell-ide-engine/pull/1324), by @fendor)

- Reuse unHTML for searchModules'
([#1323](https://github.com/haskell/haskell-ide-engine/pull/1323), by @fendor)

- add possibility to run `install.hs` from cabal
([#1221](https://github.com/haskell/haskell-ide-engine/pull/1221), by @power-fungus)


# 0.11.0.0

- Bump resolvers. `lts-13.27` for GHC 8.6.5, `nightly-2019-07-07` for
  nightly build, rest are unchanged.
([#1319 ](https://github.com/haskell/haskell-ide-engine/pull/1319),by @alanz)
([#1316 ](https://github.com/haskell/haskell-ide-engine/pull/1316), by @lorenzo)

- Clear out pattern matching and error message of executeCodeActionByName
([#1317 ](https://github.com/haskell/haskell-ide-engine/pull/1317), by @jneira)

- Upgrade to haskell-lsp 0.15
([#1316 ](https://github.com/haskell/haskell-ide-engine/pull/1316), by @lorenzo)

- Update Arch Linux install instructions
([#1315 ](https://github.com/haskell/haskell-ide-engine/pull/1315), by @friedbrice)

- Fix liquid unit test normalizing paths
([#1310 ](https://github.com/haskell/haskell-ide-engine/pull/1310), by @jneira)

- Add unix-time constraint to cabal file
([#1306 ](https://github.com/haskell/haskell-ide-engine/pull/1306), by @alanz)

- Fix a memory leak found by @mpickering
([#1305 ](https://github.com/haskell/haskell-ide-engine/pull/1305), by @lorenzo)

- Fix build for Windows 7
([#1304 ](https://github.com/haskell/haskell-ide-engine/pull/1304), by @jneira)

- Brittany 0.12
([#1301 ](https://github.com/haskell/haskell-ide-engine/pull/1301), by @alanz)

- Use ghc-mod without memory leak
([#1299 ](https://github.com/haskell/haskell-ide-engine/pull/1299), by @alanz)

- install.hs: Make all available GHCs in PATH buildable
([#1297 ](https://github.com/haskell/haskell-ide-engine/pull/1297), by @maoe)

- Fix file mapping state when we have a parsed module but not a typechecked module
([#1295 ](https://github.com/haskell/haskell-ide-engine/pull/1295), by @wz1000)

- Use ghc-mod which loads ghc plugins
([#1293 ](https://github.com/haskell/haskell-ide-engine/pull/1293), by @alanz)

- Fix UriCaches being leaked (bug fix)
([#1292 ](https://github.com/haskell/haskell-ide-engine/pull/1292), by @bubba)

- Stack 2.1.1
([#1291 ](https://github.com/haskell/haskell-ide-engine/pull/1291), by @alanz)

- Render completion documentation to markdown
([#1290 ](https://github.com/haskell/haskell-ide-engine/pull/1290), by @Avi-D-coder)

- Trying out haskell-lsp 0.14
([#1288 ](https://github.com/haskell/haskell-ide-engine/pull/1288), by @alanz)

- Hlint 2.1.24
([#1287 ](https://github.com/haskell/haskell-ide-engine/pull/1287), by @alanz)

- Improve import action of hsimport
([#1284 ](https://github.com/haskell/haskell-ide-engine/pull/1284), by @fendor)

- Add liquid haskell smt solver to README
([#1283 ](https://github.com/haskell/haskell-ide-engine/pull/1283), by @fendor)


# 0.10.0.0

- Drop GHC 8.2.1 support.
  ([#1279](https://github.com/haskell/haskell-ide-engine/pull/1279),
  @alanz)

- Bump resolvers and hoogle, LTS 13.23 for GHC 8.6.5,
  nightly-2019-05-31 for stack.yaml and hoogle version 5.0.17.9
  ([#1277](https://github.com/haskell/haskell-ide-engine/pull/1277),
  @alanz)

- HsImport importlist, Offers code action to add a function to import list.
  ([#1170](https://github.com/haskell/haskell-ide-engine/pull/1170), @fendor)

- Typemap reimplementation
  ([#1186](https://github.com/haskell/haskell-ide-engine/pull/1186), @fendor)

- Add window/progress reporting for typechecking. Note: needs LSP
  client to support a recent spec change.
  ([#1190](https://github.com/haskell/haskell-ide-engine/pull/1190),
  @bubba)

- Add package to library component in package.yaml
  ([#1237](https://github.com/haskell/haskell-ide-engine/pull/1237), @fendor)

- hie sends invalid message on hover
([#1246](https://github.com/haskell/haskell-ide-engine/pull/1246), @Hogeyama)

- Use floskell from hackage
([#1242](https://github.com/haskell/haskell-ide-engine/pull/1242), @bubba)

- Adapting to new haskell-lsp
([#1247](https://github.com/haskell/haskell-ide-engine/pull/1247), @alanz)

- Remove HoverContentsEmpty
([#1251](https://github.com/haskell/haskell-ide-engine/pull/1251), @alanz)

- Use lsp-test-0.5.2.2 from hackage
([#1252](https://github.com/haskell/haskell-ide-engine/pull/1252), @bubba)

- Use haskell-lsp-12.1.0 from hackage
([#1253](https://github.com/haskell/haskell-ide-engine/pull/1253), @alanz)

- Bump haskell-lsp to 0.13.0.0
([#1260](https://github.com/haskell/haskell-ide-engine/pull/1260), @alanz)

- Bump version for hsimport to 0.10.0
([#1265](https://github.com/haskell/haskell-ide-engine/pull/1265), @fendor)

- Revert "Revert "Merge pull request #1237 from fendor/add-package-tests""
([#1268](https://github.com/haskell/haskell-ide-engine/pull/1268), @alanz)

- Hlint 2.1.22
([#1270](https://github.com/haskell/haskell-ide-engine/pull/1270), @alanz)

- Documentation

  - Add Nix cabal-helper fix to troubleshooting section
    ([#1231](https://github.com/haskell/haskell-ide-engine/pull/1231),
    @Infinisil)

  - Troubleshooting for emacs
    ([#1240](https://github.com/haskell/haskell-ide-engine/pull/1240),
    @Infinisil)

  - Change url for nix installation instructions
    ([#1258](https://github.com/haskell/haskell-ide-engine/pull/1258),
    @malob)

- Preparations for hie-bios

  - HaRe hie plugin api
    ([#1215](https://github.com/haskell/haskell-ide-engine/pull/1215),
    @alanz)

  - Narrow ghc mod core
    ([#1255](https://github.com/haskell/haskell-ide-engine/pull/1255),
    @alanz)

- Build system (install.hs)

  - Extra argument causes cabal-build-doc to fail
    ([#1239](https://github.com/haskell/haskell-ide-engine/pull/1239),
    @bflyblue)

  - Add an explicit stack file for GHC 8.6.5
    ([#1241](https://github.com/haskell/haskell-ide-engine/pull/1241),
    @alanz)

  - Bump shake resolver
    ([#1272](https://github.com/haskell/haskell-ide-engine/pull/1272),
    @fendor)

  - Avoid legacy warning
    ([#1273](https://github.com/haskell/haskell-ide-engine/pull/1273),
    @fendor)

# 0.9.0.0

- GHC 8.6.5 preliminary support added via the nightly build (@alanz)
- Resolver bumped, LTS 13.19 for GHC 8.6.4 (@alanz)
- Add `diagnosticsOnChange` config parameter, default `True`
  (preserving prior hie behaviour). Setting it `False` only generates
  diagnostics on file save. ([#1164](https://github.com/haskell/haskell-ide-engine/pull/1164), @mpickering/@lorenzo)
- The `Hsimport` plugin now formats the resulting change using the
  formatter configured for hie. ([#1167](https://github.com/haskell/haskell-ide-engine/pull/1167),@fendor)
- Actually enable type definition requests, if supported by the client
  (e.g. vscode). ([#1169](https://github.com/haskell/haskell-ide-engine/pull/1169)/@fendor, [#1172](https://github.com/haskell/haskell-ide-engine/pull/1172)/@bubba)
- Use LSP MarkupContent for generated documentation ([#1181](https://github.com/haskell/haskell-ide-engine/pull/1181), @alanz)
- remove installation of Cabal by cabal ([#1184](https://github.com/haskell/haskell-ide-engine/pull/1184), @power-fungus)
- Add EmptyDataDecls to available pragmas, for generating code actions
  to insert if needed. ([#1187](https://github.com/haskell/haskell-ide-engine/pull/1187),@fendor)
- Make sure the end of formatted text is properly indicated for marked
  up documentation ([#1189](https://github.com/haskell/haskell-ide-engine/pull/1189), @alanz)
- Fix some of the tests with cabal new-build ([#1194](https://github.com/haskell/haskell-ide-engine/pull/1194), @michaelpj)
- Update build-tool-depends for func-test ([#1198](https://github.com/haskell/haskell-ide-engine/pull/1198), @bubba)
- Fix version of lsp-test so `cabal new-build` works ([#1211](https://github.com/haskell/haskell-ide-engine/pull/1211), @power-fungus)
- Bump hlint to 2.1.17 ([#1213](https://github.com/haskell/haskell-ide-engine/pull/1213), @alanz)
- Use cabal helper that searches with exe extension on windows ([#1217](https://github.com/haskell/haskell-ide-engine/pull/1217), @alanz)

- Stability improvements
  - Avoid crash in case of nonsensical hoogle db ([#1174](https://github.com/haskell/haskell-ide-engine/pull/1174), @fendor)
  - Prevent hie crash if apply-refact crashes ([#1220](https://github.com/haskell/haskell-ide-engine/pull/1220), @Hogeyama)

- Documentation improvements
  - Improve code documentation about formatters ([#1165](https://github.com/haskell/haskell-ide-engine/pull/1165),@fendor)
  - Add code documentation for the Hoogle plugin ([#1173](https://github.com/haskell/haskell-ide-engine/pull/1173),@fendor)
  - Change 'build-docs' to 'build-doc' in README ([#1185](https://github.com/haskell/haskell-ide-engine/pull/1185), @ajeetdsouza)
  - README Nix - replace old.postFixup -> postFixup ([#1193](https://github.com/haskell/haskell-ide-engine/pull/1193), @backuitist)
  - Expand documentation on the build system ([#1200](https://github.com/haskell/haskell-ide-engine/pull/1200), @power-fungus)
  - Fixed a typo. ([#1212](https://github.com/haskell/haskell-ide-engine/pull/1212), @rashadg1030)
  - Add documentation about building hie with profiling
    enabled. ([#1225](https://github.com/haskell/haskell-ide-engine/pull/1225), @skress)
  - Add Documentation for Pragmas Plugin ([#1222](https://github.com/haskell/haskell-ide-engine/pull/1222), @fendor)

- Build system improvements
  - Further improvements and simplification of the `./install.hs`
    build system ([#1168](https://github.com/haskell/haskell-ide-engine/pull/1168), @power-fungus)

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
 - HIE now warns you if there is mismatch between the HIE GHC version and the project GHC version
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
