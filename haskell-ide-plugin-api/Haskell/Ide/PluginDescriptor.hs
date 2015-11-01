-- | Experimenting with a data structure to define a plugin.
--
-- The general idea is that a given plugin returns this structure during the
-- initial load/registration process, when- or however this eventually happens.
--
-- It should define the following things
--  1. What features the plugin should expose into the IDE
--  2. What resources it requires access to in order to do this
--       (this one may not be needed initially)
--
--       This may include a requirement to store private data of a particular
--       form.
--
--       It may be interesting to look at the Android model wrt Intents and
--       shared resource management, e.g. default Calendar app, default SMS app,
--       all making use of Contacts service.

module Haskell.Ide.PluginDescriptor where

import qualified Data.Map as Map

-- ---------------------------------------------------------------------

data PluginDescriptor = PluginDescriptor
  { pdUiCommands :: [UiCommand]
  , pdExposedServices :: [Service]
  , pdUsedServices :: [Service]
  }

-- |Ideally a UiCommand is defined in such a way that it can be exposed via the
-- native CLI for the tool being exposed as well. Perhaps use
-- Options.Applicative for this in some way.
data UiCommand = UiCommand
  { uiCmdName :: CommandName
  , uiContexts :: [AcceptedContext] -- TODO: should this be a non empty list? or should empty list imply CtxNone.
  , uiAdditionalParams :: [RequiredParam]
  }

type CommandName = String

data Service = Service
  { svcName :: String
  -- , svcXXX :: undefined
  }

-- |Define what context will be accepted from the frontend for the specific
-- command. Matches up to corresponding values for CommandContext
data AcceptedContext = CtxNone        -- ^ No context required, global command
                     | CtxPoint       -- ^ A single (Line,Col) in a specific file
                     | CtxRegion      -- ^ A region within a specific file
                     | CtxFile        -- ^ Works on a whole file
                     | CtxCabalTarget -- ^ Works on a specific cabal target
                     deriving (Eq,Show)

-- | The actual details of a context when sent from the front-end to the plugin,
-- according to the matching AcceptContext
data CommandContext = NoContext                  -- ^ No or global context
                    | RowCol Int Int             -- ^ A single (Line,Col) in a specific file
                    | Region (Int,Int) (Int,Int) -- ^ A region within a specific file
                    | WholeFile                  -- ^ Works on the whole file
                    | CabalTarget                -- ^ Works on a specific cabal target
                    deriving (Eq,Show)

data SessionContext = CabalSession CabalSection AbsFilePath
                    | SimpleSession AbsFilePath
                    | NoSession
                    deriving (Eq,Show)

-- |It will simplify things to always work with an absolute file path
type AbsFilePath = FilePath

data CabalSection = CabalSection String deriving (Show,Eq)

-- |Initially all params will be returned as strings. This can become a much
-- richer structure in time.
data RequiredParam = RP String -- ^ Prompt

-- ---------------------------------------------------------------------

data IdeRequest = IdeRequest
  { ideCommand :: CommandName
  , ideSession :: SessionContext
  , ideContext :: CommandContext
  , ideParams  :: Map.Map ParamId ParamVal
  } deriving Show

type ParamId = String
type ParamVal = String

data IdeResponse = IdeResponseOk String
                 | IdeResponseFail String
                 | IdeResponseError String
                 deriving Show

type Dispatcher = IdeRequest -> IO IdeResponse

-- EOF
