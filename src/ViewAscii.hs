import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Control.Exception
import System.IO
import System.Environment
import System.Exit

_SPECIAL_CODES_ :: [(BS.ByteString, String)]
_SPECIAL_CODES_ =
    [(p1 0xE4      , "<iso-8859-1: a with diaresis>")
    ,(p1 0xF6      , "<iso-8859-1: o with diaresis>")
    ,(p1 0xFC      , "<iso-8859-1: u with diaresis>")
    ,(p1 0xC4      , "<iso-8859-1: A with diaresis>")
    ,(p1 0xD6      , "<iso-8859-1: O with diaresis>")
    ,(p1 0xDC      , "<iso-8859-1: U with diaresis>")
    ,(p1 0xDF      , "<iso-8859-1: sharp s>")
    ,(p2 0xC3 0xA4 , "<utf-8: a with diaresis>")
    ,(p2 0xC3 0xB6 , "<utf-8: o with diaresis>")
    ,(p2 0xC3 0xBC , "<utf-8: u with diaresis>")
    ,(p2 0xC3 0x84 , "<utf-8: A with diaresis>")
    ,(p2 0xC3 0x96 , "<utf-8: O with diaresis>")
    ,(p2 0xC3 0x9C , "<utf-8: U with diaresis>")
    ,(p2 0xC3 0x9f , "<utf-8: sharp s>")]
    where
      p1 = BS.singleton
      p2 w1 w2 = BS.pack [w1, w2]

lookupSpecial :: BS.ByteString -> Maybe (String, BS.ByteString)
lookupSpecial bs =
    lookup _SPECIAL_CODES_
    where
      lookup [] = Nothing
      lookup ((code, msg):rest) =
          let (prefix, suffix) = BS.splitAt (BS.length code) bs
          in if prefix == code
             then Just (msg, suffix)
             else lookup rest

transform :: BS.ByteString -> [BS.ByteString] -> BS.ByteString
transform chunk acc =
    let (prefix, suffix) = BS.span (\w -> w >= 0x20 && w <= 0x7E) chunk
        newAcc = prefix : acc
    in case lookupSpecial suffix of
         Nothing ->
             case BS.uncons suffix of
               Nothing ->  -- suffix ist leer
                   BS.concat (reverse newAcc)
               Just (b, rest) ->
                   let newAcc' =
                           (if b == 10
                            then BS.singleton b -- \n
                            else encode ("<" ++ show b ++ ">")) : newAcc
                   in transform rest newAcc'
         Just (special, rest) ->
             transform rest (encode special : newAcc)
    where
      encode :: String -> BS.ByteString
      encode s = T.encodeUtf8 (T.pack s)

handleFile :: FilePath -> IO ()
handleFile file =
    if file == "-"
    then action stdin
    else withFile file ReadMode action
    where
      action h = loop h `catch` (\ex -> hPutStrLn stderr (file ++ ": " ++
                                                          show (ex::IOException)))
      loop h =
          do bs <- BS.hGet h 4096
             if BS.null bs
             then return () -- EOF
             else do BS.putStr (transform bs [])
                     loop h

parseArgs :: [String] -> IO [FilePath]
parseArgs args =
    if "-h" `elem` args || "--help" `elem` args
    then usage
    else if null args
         then return ["-"]
         else return args
    where
      usage = abort "USAGE: view-ascii [FILE ...]"
      abort msg =
          do hPutStrLn stderr msg
             exitWith (ExitFailure 1)

main =
    do args <- getArgs
       files <- parseArgs args
       mapM_ handleFile files
