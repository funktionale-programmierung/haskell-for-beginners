{-# LANGUAGE OverloadedStrings #-}
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Environment

parseKeyValues :: T.Text -> [(T.Text, T.Text)]
parseKeyValues txt =
    mapMaybe parseLine (T.lines txt)
    where
      parseLine :: T.Text -> Maybe (T.Text, T.Text)
      parseLine txt =
          case T.span (/= ':') (T.stripStart txt) of
            (key, value) ->
                if "#" `T.isPrefixOf` key
                then Nothing               -- Kommentare ignorieren
                else case T.uncons value of
                       Just (_, rest) -> Just (key, T.strip rest)
                       Nothing -> Nothing  -- Fehler ignorieren

parseConfigFile :: FilePath -> IO ()
parseConfigFile fp =
    do txt <- T.readFile fp
       let keyValues = parseKeyValues txt
       putStrLn (show keyValues)

main :: IO ()
main =
    do args <- getArgs
       mapM_ parseConfigFile args
