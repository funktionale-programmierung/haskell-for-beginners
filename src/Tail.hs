import qualified Data.Sequence as Seq
import qualified Data.Foldable as F
import Safe
import System.Environment
import System.Exit
import System.IO
import qualified Data.ByteString as BS

printTail :: Int -> FilePath -> IO ()
printTail lines file =
    if file == "-"
    then doWork stdin
    else withFile file ReadMode doWork
    where
      doWork :: Handle -> IO ()
      doWork h =
          do list <- collectLines lines h
             mapM_ printLine list
      printLine bs =
          do BS.hPut stdout bs
             BS.hPut stdout newline
      newline = BS.pack [10]

collectLines :: Int -> Handle -> IO [BS.ByteString]
collectLines lines handle = loop Seq.empty
    where
      loop acc =
          do isEof <- hIsEOF handle
             if isEof
             then return (F.toList acc)
             else do l <- BS.hGetLine handle
                     loop (trim (acc Seq.|> l))
      trim seq =
          if Seq.length seq <= lines
          then seq
          else case Seq.viewl seq of
                 Seq.EmptyL -> error ("Bug in tail: argument to -n should not be negative")
                 _ Seq.:< rest -> rest

data TailOpts
    = TailOpts
      { to_files :: [FilePath]
      , to_lines :: Int
      }

parseArgs :: [String] -> IO TailOpts
parseArgs args =
    if "-h" `elem` args || "--help" `elem` args
    then usage
    else
        case args of
          ("-n" : nStr : rest) ->
              case readMay nStr of
                Just n | n >= 0 ->
                    return (TailOpts { to_files = rest, to_lines = n })
                _ ->
                    abort "Argument to -n must be a non-negative int."
          [] -> return (TailOpts { to_files = ["-"], to_lines = defaultLines })
          _ -> return (TailOpts { to_files = args, to_lines = defaultLines })
    where
      usage = abort "USAGE: tail [-n N] [FILE ...]"
      defaultLines = 10
      abort msg =
          do hPutStrLn stderr msg
             exitWith (ExitFailure 1)

main =
    do args <- getArgs
       opts <- parseArgs args
       mapM_ (printTail (to_lines opts)) (to_files opts)
