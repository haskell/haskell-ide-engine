module Haskell.Ide.PluginDescriptor where

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


data PluginDescriptor = PluginDescriptor
  { pdUiCommands :: [UiCommand]
  , pdExposedServices :: [Service]
  , pdUsedServices :: [Service]
  }

-- |Ideally a UiCommand is defined in such a way that it can be exposed via the
-- native CLI for the tool being exposed as well. Perhaps use
-- Options.Applicative for this in some way.
data UiCommands = [UiCommand]
  {
  }

-- | A service is something generic, and includes (but is not limited to)
data Service = Service
  { svcName :: String
  -- , svcXXX :: undefined
  }
