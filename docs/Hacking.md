## Hacking on haskell-ide-engine

### Getting started

```
$ git clone https://github.com/haskell/haskell-ide-engine.git
$ cd haskell-ide-engine
$ stack build
```

There are multiple stack files, one for each currently supported compiler.

To build and test with all of them, use

```
make test
```

To see what version/compiler you are running

```bash
hie --version
Version 0.1.0.0, Git revision 54338553f9c07b7d3427a769f7f4c2c366cefc7f (dirty) (1218 commits) x86_64 ghc-8.2.2
```


### Plugins (Old architecture)

__Note: this is only used for the executeCommand request in the LSP transport. The new plugin architecture used for LSP is described in [Architecture](Architecture.md)__

A plugin needs to provide a `PluginDescriptor` which exposes the available commands in it.

The one for the `base` plugin is

```haskell
baseDescriptor :: PluginDescriptor
baseDescriptor = PluginDescriptor
  {
    pdUiCommands =
      [
        UiCommand
          { uiCmdName = "version"
          , uiContexts = [CtxNone]
          , uiAdditionalParams = []
          , uiFunc = versionCmd
          }
      , UiCommand
          { uiCmdName = "plugins"
          , uiContexts = [CtxNone]
          , uiAdditionalParams = []
          , uiFunc = pluginsCmd
          }
      , UiCommand
          { uiCmdName = "commands"
          , uiContexts = [CtxNone]
          , uiAdditionalParams = [RP "plugin"]
          , uiFunc = commandsCmd
          }
      , UiCommand
          { uiCmdName = "pwd"
          , uiContexts = [CtxNone]
          , uiAdditionalParams = []
          , uiFunc = pwdCmd
          }
      , UiCommand
          { uiCmdName = "cwd"
          , uiContexts = [CtxNone]
          , uiAdditionalParams = [RP "dir"]
          , uiFunc = cwdCmd
          }
      ]
  , pdExposedServices = []
  , pdUsedServices    = []
  }
  ```

The last two entries are currently unused

This plugin is made active in `MainHie.hs` by

```haskell
plugins :: Plugins
plugins = Map.fromList
  [
    -- Note: statically including known plugins. In future this map could be set
    -- up via a config file of some kind.
    ("eg2", example2Descriptor)
    -- The base plugin, able to answer questions about the IDE Engine environment.
  , ("base", baseDescriptor)
  ]
```

### Plugin Types

The types related to a plugin are defined in a separate lightweight package,
`haskell-ide-plugin-api`. This is so that it can be included in any tool (e.g.
HaRe) and the tool can then expose a `PluginDescriptor`. All that `HIE` needs to
do is put the tool into its cabal file as a dependency and fill in an entry in
the `plugins` table.

### Operation

When a request is recived from any of the frontends, this is routed to the
central dispatcher via a `Chan`. Based on the specified plugin name and
`IdeRequest` `ideCommand` value the appropriate `UiCommand` is identified and
its `uiFunc` is called.

The `uiFunc` is of type `Dispatcher`, which is defined as

```haskell
type Dispatcher = forall m. (MonadIO m,GHC.GhcMonad m,HasIdeState m) => IdeRequest -> m IdeResponse
```

This type is not the monad used in `HIE`, but does guarantee that it can access
IO and the `HIE` state, which is currently only the table of plugins.

It also makes the `GhcMonad` available. In a fully general version this my not
be necessary.
