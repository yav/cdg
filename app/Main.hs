module Main where

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Binary(decodeOrFail)
import Control.Monad(foldM_,foldM)
import System.Environment(getArgs)
import System.Directory(createDirectoryIfMissing)
import System.FilePath((</>),(<.>))
import Codec.Picture qualified as J

import CDG
import CDG.Constants
import CDG.Eval


main :: IO ()
main =
 do args <- getArgs
    case args of
      [file] ->
        processFile =<< BS.readFile file
      _ -> putStrLn "Need a file"


saveState :: FilePath -> State -> IO ()
saveState file s = J.savePngImage file (J.ImageRGBA8 (render s))

test :: IO ()
test = saveState "test.png" s4
  where
  s1 = doCommand (ClearScreen (ColorIndex 3) 0) initialState
  s2 = doCommand (ClearBorder (ColorIndex 4)) s1
  s3 = doCommand (SetTile tile) s2
  tile = Tile { fg = ColorIndex 15, bg = ColorIndex 8, row = 1, col = 1
              , bitmap = BS.pack (replicate 12 0xFF) }
  s4 = iterate (doCommand (ScrollCopy (Scroll hs vs))) s3 !! 5
  hs = ScrollAmt 1 0
  vs = ScrollAmt 0 0

-- | The time at a particular packet
timeInMilli :: Int -> Int
timeInMilli packetNum = (1000 * packetNum) `quot` packetsPerSec

showTimeMilli :: Int -> String
showTimeMilli millis = show mins ++ ":" ++ show ss ++ ":" ++ show ms
  where
  (secs,ms)  = millis `quotRem` 1000
  (mins,ss)  = secs `quotRem` 60

processFile :: BS.ByteString -> IO ()
processFile bs =
  do createDirectoryIfMissing True dir
     (_,_,is,_) <- foldM doPacket (0,0,[],initialState) (take packets [ 0 .. ])
     let is' = map (J.convertRGB8 . J.ImageRGBA8) (reverse is)
     case J.writeGifAnimation "x.gif" 5 J.LoopingNever is' of
       Right ok -> ok
       Left err -> print err
     pure ()
  where
  dir           = "out"
  outFile i     = dir </> ("frame_" ++ show i) <.> "png"

  len           = BS.length bs
  packets       = len `quot` packetSize
  packetBytes i = LBS.fromStrict (BS.take packetSize (BS.drop (i * packetSize) bs))
  doPacket (!done,lastSave,is,!s) i =
   do -- putStrLn ("Time " ++ showTimeMilli (timeInMilli done))
      case decodeOrFail (packetBytes i) of
        Left {} -> pure (done + 1, lastSave, is, s)
        Right (_,off,cmd)
          | off /= 24 -> error $ unlines [ "didn't read all bytes"
                                         , show off
                                         , show cmd ]
          | otherwise ->
          do let s1 = doCommand cmd s
             let t = timeInMilli done
                 frames = (t - lastSave) `quot` 50
             pure
               if frames > 0
                  then (done + 1, t, replicate frames (render s1) ++ is, s1)
                  else (done + 1, lastSave, is, s1)
