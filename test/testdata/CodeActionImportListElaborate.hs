{-# LANGUAGE NoImplicitPrelude #-}
import           System.IO (IO)
-- | Main entry point to the program
main :: IO ()
main =
    when True
        $ hPutStrLn stderr
        $ fromMaybe "Good night, World!" (Just "Hello, World!")