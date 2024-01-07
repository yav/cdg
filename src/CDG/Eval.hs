module CDG.Eval (State(..), initialState, doCommand, render) where

import Data.Vector(Vector)
import Data.Vector qualified as V
import Codec.Picture qualified as J

import CDG
import CDG.Constants

data State = State
  { colorTable        :: !ColorTable
  , transparentColor  :: !(Maybe ColorIndex)
  , hOffset, vOffset  :: !Int
  , screen            :: !(Vector Sprite)
  } deriving Show

initialState :: State
initialState = State
  { colorTable       = ansiColors
  , transparentColor = Nothing
  , hOffset          = 0
  , vOffset          = 0
  , screen           = V.replicate (screenGridWidth * screenGridHeight)
                                   (fillSprite zeroColorIndex)
  }

render :: State -> J.Image J.PixelRGBA8
render s = J.generateImage pixel screenVisibleWidth screenVisibleHeight
  where
  pixel col row        = colorIxToPixel s (colorIndexAt row col)
  colorIndexAt row col = colorIndexAt' (row + vBorder + vOffset s)
                                       (col + hBorder + hOffset s)
  colorIndexAt' row col =
    let (grow,srow) = row `quotRem` tileHeight
        (gcol,scol) = col `quotRem` tileWidth
        sprite      = getSprite grow gcol (screen s)
    in spriteColorAt srow scol sprite


colorIxToPixel :: State -> ColorIndex -> J.PixelRGBA8
colorIxToPixel s i =
  case transparentColor s of
    Just t | i == t -> J.PixelRGBA8 0 0 0 0
    _ -> let rgb = getColor i (colorTable s)
             cvt f = fromIntegral (f rgb)
         in J.PixelRGBA8 (cvt red) (cvt green) (cvt blue) 0xFF



--------------------------------------------------------------------------------
doCommand :: Command -> State -> State
doCommand cmd s =
  case cmd of
    ClearScreen ci rpt
      | rpt > 0         -> s
      | otherwise       -> s { screen = fillSprite ci <$ screen s }
    ClearBorder ci      -> updateScreen (clearBorder (fillSprite ci)) s
    SetTile  t          -> updateScreen (setTile False t) s
    XorTile t           -> updateScreen (setTile True t) s
    ScrollColor ci sc   -> doScroll (Just (fillSprite ci)) sc s
    ScrollCopy sc       -> doScroll Nothing sc s
    SetTransparent ci   -> s { transparentColor = Just ci }
    LoadColorsLo ct     -> s { colorTable = setColorLo ct (colorTable s) }
    LoadColorsHi ct     -> s { colorTable = setColorHi ct (colorTable s) }


updateScreen :: (Int -> Int -> Sprite -> Sprite) -> State -> State
updateScreen f s = s { screen = V.imap f' (screen s) }
  where
  f' i c =
    let (row,col) = i `quotRem` screenGridWidth
    in f row col c

clearBorder :: Sprite -> Int -> Int -> Sprite -> Sprite
clearBorder new row col cur = if isBorder row col then new else cur

isBorder :: Int -> Int -> Bool
isBorder row col = row == 0 || row == screenGridLastRow
                || col == 0 || col == screenGridLastColumn

setTile :: Bool -> Tile -> Int -> Int -> Sprite -> Sprite
setTile doXor tile prow pcol cur
  | prow == row tile && pcol == col tile =
    let new = tileSprite tile
    in if doXor then xorSprite new cur else new
  | otherwise = cur

getSprite :: Int -> Int -> Vector Sprite -> Sprite
getSprite row col scr =
  case scr V.!? (r * screenGridWidth + c) of
    Just s  -> s
    Nothing -> error "sprite not on screen" -- fillSprite zeroColorIndex
  where
  r = (row + screenGridHeight) `rem` screenGridHeight
  c = (col + screenGridWidth) `rem` screenGridWidth

doScroll :: Maybe Sprite -> Scroll -> State -> State
doScroll mb Scroll { .. } s =
  s1 { vOffset = offset ver, hOffset = offset hor }
  where
  dx        = scroll hor
  dy        = scroll ver
  s1        = if dx == 0 && dy == 0 then s else updateScreen upd s
  upd r c _ =
    case mb of
      Just ic
        |  r == 0 && dy == 1
        || r == screenGridLastRow && dy == -1
        || c == 0 && dx == 1
        || c == screenGridLastColumn && dy == -1
        -> ic

      _ -> getSprite (r - dy) (c - dx) (screen s)




