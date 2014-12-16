{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Aeson as J

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as BS

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Vector as V

import Data.Maybe
import Text.PrettyPrint

import Test.Framework

import System.IO
import System.Environment
import System.Exit

type JsonPath = [T.Text]

filterJson :: JsonPath -> J.Value -> Maybe J.Value
filterJson path json =
    case path of
      [] -> Just json
      (p:ps) ->
          case json of
            J.Object m ->
                case HashMap.lookup p m of
                  Nothing -> Nothing
                  Just v -> filterJson ps v
            J.Array arr ->
                let newArr = V.fromList (mapMaybe (filterJson path) (V.toList arr))
                in Just (J.Array newArr)
            _ -> Nothing

prettyJson :: J.Value -> Doc
prettyJson json =
    case json of
      J.Object m ->
          let elems = map prettyKv (HashMap.toList m)
          in braces (prettyList elems)
      J.Array arr ->
          let elems = map prettyJson (V.toList arr)
          in brackets (prettyList elems)
      J.String t -> prettyString t
      J.Number n -> text (show n)
      J.Bool b -> text (if b then "true" else "false")
      J.Null -> text "null"
    where
      prettyList l =
          nest 1 (vcat (punctuate comma l))
      prettyKv (k, v) =
          let combine x y =
                  if isStructured v
                  then x $$ (nest 1 y)
                  else x <+> y
          in (prettyString k <> text ":") `combine` prettyJson v
      prettyString t =
          text (show t)
      isStructured json =
          case json of
            J.Object _ -> True
            J.Array _ -> True
            _ -> False

processFile :: JsonPath -> FilePath -> IO ()
processFile filter file =
    if file == "-" then action stdin else withFile file ReadMode action
    where
      action handle =
          do bytes <- BS.hGetContents handle
             case J.decodeStrict bytes of
               Just v ->
                   case filterJson filter v of
                     Nothing -> return ()
                     Just outV -> putStrLn (show (prettyJson outV))
               Nothing ->
                   hPutStrLn stderr ("Error parsing JSON from " ++ file)

main :: IO ()
main =
    do args <- getArgs
       if ("--help" `elem` args || "-h" `elem` args) then usage else return ()
       case args of
         "--test":rest -> runTests rest
         "--filter":filter:files -> doWork (parseFilter filter) files
         "--filter":[] -> usage
         files -> doWork [] files
    where
      parseFilter :: String -> JsonPath
      parseFilter str =
          T.splitOn "." (T.pack str)
      usage :: IO a
      usage =
          do hPutStrLn stderr ("USAGE: jsonpp [--test] [--filter EXPR] [FILE..]")
             exitWith (ExitFailure 1)
      doWork :: JsonPath -> [FilePath] -> IO ()
      doWork filter files =
          if null files
          then processFile filter "-"
          else mapM_ (processFile filter) files
      runTests :: [String] -> IO ()
      runTests args = htfMainWithArgs args htf_thisModulesTests

--
-- Tests
--
test_filterJson =
    do assertEqual (Just $ unsafeParseJson "[36, 23]") (filterJson ["age"] sampleJson)
       assertEqual (Just $ unsafeParseJson "[\"Stefan\"]") (filterJson ["name", "first"] sampleJson)
       assertEqual Nothing (filterJson ["name"] (J.Number 42))
       assertEqual (Just sampleJson) (filterJson [] sampleJson)

unsafeParseJson :: T.Text -> J.Value
unsafeParseJson t =
    case J.decodeStrict (T.encodeUtf8 t) of
      Just v -> v
      Nothing -> error ("Could not parse JSON: " ++ T.unpack t)

sampleJson :: J.Value
sampleJson =
    unsafeParseJson
      (T.concat
        ["[{\"name\": {\"first\": \"Stefan\", \"last\": \"Wehr\"}, \"age\": 36},",
         " {\"name\": \"Max\", \"age\": 23}]"])

test_prettyJson =
    do assertEqual expected (show (prettyJson sampleJson))
    where
      expected =
          ("[{\"age\": 36.0,\n" ++
           "  \"name\":\n" ++
           "   {\"first\": \"Stefan\",\n" ++
           "    \"last\": \"Wehr\"}},\n" ++
           " {\"age\": 23.0,\n" ++
           "  \"name\": \"Max\"}]")
