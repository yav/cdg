module Main where

import System.Environment(getArgs)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Binary(decodeOrFail)

import CDG


main :: IO ()
main =
 do args <- getArgs
    case args of
      [file] ->
        processFile =<< BS.readFile file
      _ -> putStrLn "Need a file"


processFile :: BS.ByteString -> IO ()
processFile bs = mapM_ doPacket (take packets [ 0 .. ])
  where
  len           = BS.length bs
  packets       = len `quot` packetSize
  packetBytes i = LBS.fromStrict (BS.take packetSize (BS.drop (i * packetSize) bs))
  doPacket i =
   do putStr ("Packet " ++ show i)
      case decodeOrFail (packetBytes i) of
        Left {}       -> putStrLn "NO"
        Right (_,_,a) -> print (a :: Command)
