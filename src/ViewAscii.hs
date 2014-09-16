import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Control.Exception
import Data.Word
import System.IO
import System.Environment
import System.Exit

_SPECIAL_CODES_ :: [(BS.ByteString, BS.ByteString)]
_SPECIAL_CODES_ =
    [(p1 0xE4      , BSC.pack "<iso-8859-1: a with diaresis>")
    ,(p1 0xF6      , BSC.pack "<iso-8859-1: o with diaresis>")
    ,(p1 0xFC      , BSC.pack "<iso-8859-1: u with diaresis>")
    ,(p1 0xC4      , BSC.pack "<iso-8859-1: A with diaresis>")
    ,(p1 0xD6      , BSC.pack "<iso-8859-1: O with diaresis>")
    ,(p1 0xDC      , BSC.pack "<iso-8859-1: U with diaresis>")
    ,(p1 0xDF      , BSC.pack "<iso-8859-1: sharp s>")
    ,(p2 0xC3 0xA4 , BSC.pack "<utf-8: a with diaresis>")
    ,(p2 0xC3 0xB6 , BSC.pack "<utf-8: o with diaresis>")
    ,(p2 0xC3 0xBC , BSC.pack "<utf-8: u with diaresis>")
    ,(p2 0xC3 0x84 , BSC.pack "<utf-8: A with diaresis>")
    ,(p2 0xC3 0x96 , BSC.pack "<utf-8: O with diaresis>")
    ,(p2 0xC3 0x9C , BSC.pack "<utf-8: U with diaresis>")
    ,(p2 0xC3 0x9f , BSC.pack "<utf-8: sharp s>")]
    where
      p1 :: Word8 -> BS.ByteString
      p1 = BS.singleton
      p2 :: Word8 -> Word8 -> BS.ByteString
      p2 w1 w2 = BS.pack [w1, w2]

lookupSpecial :: BS.ByteString -> Maybe (BS.ByteString, BS.ByteString)
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
                            else BSC.pack ("<" ++ show b ++ ">")) : newAcc
                   in transform rest newAcc'
         Just (special, rest) ->
             transform rest (special : newAcc)

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

data ViewAsciiOpts
    = ViewAsciiOpts
      { to_files :: [FilePath]
      }

parseArgs :: [String] -> IO ViewAsciiOpts
parseArgs args =
    if "-h" `elem` args || "--help" `elem` args
    then usage
    else if null args
         then return (ViewAsciiOpts ["-"])
         else return (ViewAsciiOpts args)
    where
      usage = abort "USAGE: view-ascii [FILE ...]"
      abort msg =
          do hPutStrLn stderr msg
             exitWith (ExitFailure 1)

main =
    do args <- getArgs
       opts <- parseArgs args
       mapM_ handleFile (to_files opts)
