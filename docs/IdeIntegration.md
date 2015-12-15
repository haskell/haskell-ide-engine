## Integrating with an IDE

### Connection Transport

HIE is transport-agnostic, so a variety of different connection methods are possible

* JSON/stdio - The request/response messages are streamed over stdio using `STX` as a delimiter.
* JSON/HTTP
* File an issue for a new transport, and/or submit a PR

## Channel protocol

Each transport wraps the channel protocol, which is defined by the following types

```haskell
type RequestId = Int

data ChannelRequest = CReq
  { cinPlugin    :: PluginId
  , cinReqId     :: RequestId -- ^An identifier for the request, can tie back to
                              -- e.g. a promise id. It is returned with the
                              -- ChannelResponse.
  , cinReq       :: IdeRequest
  , cinReplyChan :: Chan ChannelResponse
  } deriving Show

data ChannelResponse = CResp
  { couPlugin :: PluginId
  , coutReqId :: RequestId
  , coutResp  :: IdeResponse
  } deriving Show
```

In the `ChannelRequest` the `PluginId` is the name of the plugin the request is being routed to. This effectively creates a logical channel per plugin through HIE.

The `RequestId` is simply a number which can be used to tie the `ChannelResponse` to the original request, or be used for polling in a promise type environment.

The `cinReplyChan :: Chan ChannelResponse` is managed by the transport endpoint in HIE, and is never sent over the wire.  It has two complementary functions

1. It provides a route back from the dispatcher to the process handling the transport, so that all requests received via a particular transport are returned via the same one.

2. It allows asynchronous processing (still needs to be supported in dispatcher). A transport can potentially generate a new reply channel based on any criteria, which can then be forked off in the dispatcher.

The channel protocol together with the dispatcher creates a logical channel to the specific plugin being addressed.

### Plugin Protocol

A plugin defines an exposed interface via a `PluginDescriptor`, and can then receive `IdeRequest`s that comply with this interface.

An `IdeRequest` is defined as

```haskell
data IdeRequest = IdeRequest
  { ideCommand :: !CommandName
  , ideParams  :: !ParamMap
  } deriving (Show,Generic)
```

Where the `ParamMap` is just a map of parameter name to parameter value.

The available commands for a given plugin are made available to the IDE by calling functions in the `base` plugin, which is built into HIE.

The commands it offers are

* version : returns the HIE version
* plugins : list available plugins
* commands : list available commands for a given plugin
* commandDetail :list parameters required for a given command

The `commandDetail` command returns the following, for itself

```haskell
    CommandDesc
      { cmdName = "commandDetail"
      , cmdUiDescription = "list parameters required for a given command"
      , cmdFileExtensions = []
      , cmdContexts = [CtxNone]
      , cmdAdditionalParams = [RP "plugin"  "the plugin name"  PtText
                              ,RP "command" "the command name" PtText]
      }
```
The intention is that with these `base` plugin query commands, and the `CommandDescriptor` structure an IDE can easily discover what plugins are loaded, and what commands they expose.

The `cmdContexts` is a list of editor contexts that the command requires, which maps down to specific parameters in the  `IdeRequest` for the command.

The contexts defined are

```haskell
data AcceptedContext = CtxNone        -- ^ No context required, global command
                     | CtxFile        -- ^ Works on a whole file
                     | CtxPoint       -- ^ A single (Line,Col) in a specific file
                     | CtxRegion      -- ^ A region within a specific file
                     | CtxCabalTarget -- ^ Works on a specific cabal target
                     | CtxProject     -- ^ Works on a the whole project
                     deriving (Eq,Show,Generic)
```
The mapping from accepted context to required parameters in the request is defined as

```haskell
-- |For a given 'AcceptedContext', define the parameters that are required in
-- the corresponding 'IdeRequest'
contextMapping :: AcceptedContext -> [ParamDescription]
contextMapping CtxNone        = []
contextMapping CtxFile        = [fileParam]
contextMapping CtxPoint       = [fileParam,startPosParam]
contextMapping CtxRegion      = [fileParam,startPosParam,endPosParam]
contextMapping CtxCabalTarget = [cabalParam]
contextMapping CtxProject     = [fileParam]

fileParam :: ParamDescription
fileParam = RP "file" "a file name" PtFile

startPosParam :: ParamDescription
startPosParam = RP "start_pos" "start line and col" PtPos

endPosParam :: ParamDescription
endPosParam = RP "end_pos" "end line and col" PtPos

cabalParam :: ParamDescription
cabalParam = RP "cabal" "cabal target" PtText
```
