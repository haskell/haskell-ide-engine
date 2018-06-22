# Dispatch architecture
```
                                    Message from
                                    client
            +                             +                                +
            |                             |                                |
            |                       +-----v-----+                          |
            |                       |           |                          |
            <-------+IdeRequest<----+  Reactor  +------->GhcRequest+------->
            |                       |           |                          |
            |                       +-----------+                          |
            |                                                              |
            |                                                              |
           IdeM                                                         IdeGhcM      +--------+
            |             +----->IdeResponseDeferred                       |         |        |
 plugins <--+             |                   +                            +--------->        |
    |       |             |                   |                            |         |  ghc   |
    +------->             +                   |                            |         |  mod   |
            +------->IdeResponse              |    Triggers<----+          <---------+        |
            |            +    ^               |       |         |          |         |        |
            |            |    |           +---v-------v--+      |          |         |        |
            |            |    |           |              |      +          |         +--------+
            |            |    +-----------+ RequestQueue |  cacheModule<---+
            |            |                |              |                 |
            |            |                +--------------+                 |
            |            |                                                 |
            |            |  +----------------------------------------------+
            |            v  v                                              |
            |         IdeResult                                            |
            |          +    +                                              |
            |          v    |                                              |
            | IdeResultFail |                                              |
            |               v                                              |
            |          IdeResultOk                                         |
            |               +                                              |
            |               v                                              |
            |          RequestCallback                                     |
            v               +                                              v
                            v
                    Possibly respond to
                    client if needed
```

## Reactor
Any notifications, requests or response messages from the client are handled
through the `Reactor` which lives inside `LspStdio.hs`. It routes these messages
to call the appropriate plugins by making `PluginRequest`s, and then responds
to the client if needed. `PluginRequest`s come in 2 flavours: `IdeRequest`
and `GhcRequest`, which take place on 2 different threads.

## Threads
The 2 threads are represented by 2 monads: `IdeM` and `IdeGhcM`.

`IdeM` should be used for anything that does not require immediate access
to ghc-mod, since `IdeGhcM` is used mainly for typechecking modules and is
the most constrained resource.

## Requests
To get onto the threads, send an `IReq` or `GReq` to the dispatcher with
`makeRequest` inside `LspStdio.hs:reactor`.

## Responses
If you are on `IdeM` and need access to a cached module, consider using an
`IdeResponseDeferred`. You can provide it with a `FilePath` to a module you
want, and your callback will be called with the module whenever `IdeGhcM`
has loaded it.
It is advised that you do not return an `IdeResponseDeferred` directly though,
and instead use one of the `withCachedModule` or `withCachedModuleAndData`
helper functions instead.

## Results
An `IdeResponse` will eventually return an `IdeResult` at the end of the day,
which will hand back data to the Reactor. `IdeGhcM` skips the response part
and returns an `IdeResult` directly since it will never need to be deferred.