module Main where

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Binary(decodeOrFail)
import System.Environment(getArgs)
import Codec.Picture qualified as J

import Graphics.Gloss.Interface.Pure.Simulate qualified as G
import Graphics.Gloss.Juicy(fromImageRGBA8)

import CDG
import CDG.Constants
import CDG.Eval

main :: IO ()
main =
 do args <- getArgs
    case args of
      [file] ->
        play =<< BS.readFile file
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

play :: BS.ByteString -> IO ()
play bs = G.simulate disp G.black packetsPerSec initState seeState nextState
  where
  disp = G.InWindow "K"  (screenVisibleWidth,screenVisibleHeight) (0,0)
  initState = (bs, initialState)
  nextState _ _ (!bytes,!s) =
    ( BS.drop 24 bytes
    , case decodeOrFail (LBS.fromStrict bytes) of
        Left {}         -> s
        Right (_,_,cmd) -> doCommand cmd s
    )

  seeState (_,s) = fromImageRGBA8 (render s)

