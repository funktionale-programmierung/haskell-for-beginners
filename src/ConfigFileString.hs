import Data.Char
import Data.Maybe
import qualified Data.List as List
import System.Environment

parseKeyValues :: String -> [(String, String)]
parseKeyValues str =
    mapMaybe parseLine (lines str)
    where
      parseLine :: String -> Maybe (String, String)
      parseLine str =
          case List.span (/= ':') (stripStart str) of
            ('#':_, _) -> Nothing -- Kommentare ignorieren
            (key, ':':rest) -> Just (key, strip rest)
            _ -> Nothing          -- Fehler ignorieren
      stripStart :: String -> String
      stripStart = dropWhile isSpace
      strip :: String -> String
      strip = reverse . stripStart . reverse . stripStart

parseConfigFile :: FilePath -> IO ()
parseConfigFile fp =
    do str <- readFile fp
       let keyValues = parseKeyValues str
       putStrLn (show keyValues)

main :: IO ()
main =
    do args <- getArgs
       mapM_ parseConfigFile args
