## Overall Architecture

When launched in lsp mode, HIE creates four threads - 

 1. Gets input from the IDE via stdin, parses it via haskell-lsp 
    and forwards it to the HIE messaging loop
 2. Main messaging loop
 3. Dispatcher loop to execute `IdeM` requests
 4. Sends responses to the IDE via stdout
 
These threads communicate with each other mainly through `TChan`s and `TVar`s.
 
Due to limitations with ghc-mod and maintaining the ghc session, we are restricted to
a single thread using which we can run Plugin Requests in the `IdeM` monad

```haskell
type IdeM = IdeT IO
type IdeT m = GM.GhcModT (StateT IdeState m)
```

## Plugins and the IdeM Monad

Plugins define `IdeM` actions to be executed in the dispatcher.

The `IdeState` accessible within IdeM contains many useful things for
plugin writers, most important of which is the `CachedModule`.

```haskell
data CachedModule = CachedModule
  { tcMod       :: !TypecheckedModule
  , revMap      :: FilePath -> FilePath
  , newPosToOld :: Position -> Maybe Position
  , oldPosToNew :: Position -> Maybe Position
  }

getCachedModule :: Uri -> IdeM (Maybe CachedModule)
```

On every file open or edit, HIE tries to load a `TypecheckedModule`(as defined in the ghc api)
for the corresponding file. If this succeeds, the result is stored in the `CachedModule`.

If the module fails to load for any reason(parse error, type error etc.), the previous 
`CachedModule` for the file would still be available. __This allows HIE to respond to queries 
even when the current version of the file doesn't compile.__

In this scenario, the `newPosToOld` and `oldPosToNew` functions help to associate 
positions in the current version of the document (which doesn't compile) to the most recent
version of the document that did compile. `newPosToOld` will take a `Position` in the current
document, and give us the corresponding `Position` in the document corresponding to the
typechecked module we have, and `oldPosToNew` will take a `Position` in document corresponding
to the current typechecked module, and give us the corresponding `Position` in the current
version of the document.

These functions can fail - `newPosToOld` will fail when called with some position inside text
that has been recently inserted in the document, and `oldPosToNew` will fail when called with
some position inside text that has been deleted from the document.

We use `ghc-mod`s "mapped files" feature in order to support live queries for documents whose
contents haven't been saved to disk yet. ghc-mod creates temporary files to hold the current
version of the document. This means that every `SrcSpan` inside the `TypecheckedModule`
contains the path to the temporary file instead of the actual file on disk. `revMap` allows us
to recover the original `FilePath` given the `FilePath` of the temp file.

HIE also supports caching custom data along with the `TypecheckedModule`. This is useful
for avoiding duplicating work across queries:

```haskell
class Typeable a => ModuleCache a where
    cacheDataProducer :: CachedModule -> IdeM a

withCachedModuleAndData :: forall a b. ModuleCache a
  => Uri -> IdeM b -> (CachedModule -> a -> IdeM b) -> IdeM b
withCachedModuleAndData uri noCache callback = ...
```

This is used in `HaRePlugin` to keep a `Map` that associates `SrcSpan`s in the 
`TypecheckedModule` to their corresponding names and vice versa.

```haskell
data NameMapData = NMD
  { nameMap        :: !(Map.Map SrcSpan Name)
  , inverseNameMap ::  Map.Map Name [SrcSpan]
  } deriving (Typeable)

invert :: (Ord k, Ord v) => Map.Map k v -> Map.Map v [k]
invert m = Map.fromListWith (++) [(v,[k]) | (k,v) <- Map.toList m]

instance ModuleCache NameMapData where
  cacheDataProducer cm = pure $ NMD nm inm
    where nm  = initRdrNameMap $ tcMod cm
          inm = invert nm
```

This data is used to find all references to a symbol, and to find the name corresponding to
a particular position in the source.

```haskell
getReferencesInDoc :: Uri -> Position -> IdeM (IdeResponse [J.DocumentHighlight])
getReferencesInDoc uri pos = do
  let noCache = return $ nonExistentCacheErr "getReferencesInDoc"
  withCachedModuleAndData uri noCache $
    \cm NMD{nameMap, inverseNameMap} -> ...
```

`withCachedModuleAndData` ensures that the cached data(`NameMapData` in this case) is only
generated once per `TypecheckedModule`. It looks up the `typeRep` of the kind of data requested
in its cache, and returns the associated data if found, otherwise uses `cacheDataProducer` to
generate the data. `noCache` is called when the requested module isn't cached in the `IdeState`

This cache is automatically invalidated whenever a new `TypecheckedModule` is loaded, and
fresh data is generated when first requested.

## Dispatcher and messaging

```haskell
dispatcherP :: DispatcherEnv -> TChan PluginRequest -> IdeM ()

data PluginRequest = forall a. PReq
  { pinDocVer    :: Maybe (J.Uri,Int)
  , pinLspReqId  :: Maybe J.LspId
  , pinCallback  :: a -> IO ()
  , pinReq       :: IdeM a
  }
```

`dispatcherP`(thread #3) listens for `PluginRequest`s on the `TChan` and executes the 
`pinReq`, sending the result to the `pinCallback`. `pinDocVer` and `pinLspReqId` help us 
make sure we don't execute a stale request or a request that has been cancelled by the IDE. 
Note that because of the single threaded architecture, we can't cancel a request that 
has already started execution.

These requests are constructed by the message loop(#2). For example, here is the code
for handling the "definition" request

```haskell
-- LspStdio.hs
   ... 
   case inval of
      ... 
      Core.ReqDefinition req -> do
        liftIO $ U.logs $ "reactor:got DefinitionRequest:" ++ show req
        let params = req ^. J.params
            doc = params ^. J.textDocument . J.uri
            pos = params ^. J.position
        callback <- hieResponseHelper (req ^. J.id) $ \loc -> do
            let rspMsg = Core.makeResponseMessage req loc
            reactorSend rspMsg
        let hreq = PReq Nothing (Just $ req ^. J.id) callback $ HaRe.findDef doc pos
        makeRequest hreq
      ...

-- HaRePlugin.hs
findDef :: Uri -> Position -> IdeM (IdeResponse Location)
```

The request uses the `findDef` function in the `HaRe` plugin to get the `Location` 
of the definition of the symbol at the given position. The callback makes a LSP 
response message out of the location, and forwards it to thread #4 which sends
it to the IDE via stdout

