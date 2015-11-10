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

To be continued
