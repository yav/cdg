module CDG where

import Control.Monad(guard,replicateM_, replicateM)
import Data.Binary
import Data.ByteString(ByteString)
import Data.ByteString qualified as BS
import Data.Vector(Vector)
import Data.Vector qualified as V
import Data.Bits

-- | In bytes
packetSize :: Int
packetSize = 24

-- | In pixels
tileWidth :: Int
tileWidth = 6

-- | In pixels
tileHeight :: Int
tileHeight = 12

-- | In tiles
screenGridWidth :: Int
screenGridWidth = 50

-- | In tiles
screenGridHeight :: Int
screenGridHeight = 18

-- | In pixels
screenFullWidth :: Int
screenFullWidth = screenGridWidth * tileWidth

-- | In pixels
screenFullHeight :: Int
screenFullHeight = screenGridHeight * tileHeight

packetsPerSec :: Int
packetsPerSec = 300


data Command =
    ClearScreen     !ColorIndex !Int
  | ClearBorder     !ColorIndex
  | SetTile         !Tile
  | XorTile         !Tile
  | ScrollColor     !ColorIndex !Scroll
  | ScrollCopy      !Scroll
  | SetTransparent  !ColorIndex
  | LoadColorsLo    !ColorTable
  | LoadColorsHi    !ColorTable
    deriving Show

data ColorRGB       = ColorRGB { red, green, blue :: Int }
  deriving Show

newtype ColorIndex  = ColorIndex Int
  deriving Show

newtype ColorTable  = ColorTable (Vector ColorRGB)
  deriving Show

data Tile = Tile
  { fg     :: !ColorIndex
  , bg     :: !ColorIndex
  , row    :: !Int
  , col    :: !Int
  , bitmap :: !ByteString     -- ^ 12 rows, 6 bit/columns
  } deriving Show

data ScrollTo = None | Next | Prev
  deriving Show

data ScrollAmt = ScrollAmt
  { scroll :: !ScrollTo
  , offset :: !Int
  } deriving Show

data Scroll = Scroll
  { hor   :: !ScrollAmt
  , ver   :: !ScrollAmt
  } deriving Show

--------------------------------------------------------------------------------

instance Binary Command where
  put cmd =
   do putWord8 9
      let (tag,putPayload) = putCommand cmd
      putWord8 tag
      pad 2
      putPayload
      pad 4

  get =
   do ty <- getWord6
      guard (ty == 9)
      tag <- getWord6
      skip 2
      cmd <- getCommand tag
      skip 4
      pure cmd

pad :: Int -> Put
pad n = replicateM_ n (putWord8 0)

skip :: Int -> Get ()
skip n = replicateM_ n getWord8

putExactly :: Binary a => Int -> a -> [a] -> Put
putExactly want a xs = mapM_ put (take want (xs ++ repeat a))

getWord4 :: Get Int
getWord4 = fromIntegral . word4 <$> getWord8

getWord6 :: Get Int
getWord6 = fromIntegral . word6 <$> getWord8

putWord4 :: Int -> Put
putWord4 = putWord8 . word4 . fromIntegral

putWord6 :: Int -> Put
putWord6 = putWord8 . word6 . fromIntegral

word4 :: Word8 -> Word8
word4 x = x .&. 0x0F

word6 :: Word8 -> Word8
word6 x = x .&. 0x3F

clamp :: Int -> Int -> Int -> Int
clamp x y a
  | a < x     = x
  | a > y     = y
  | otherwise = a



putCommand :: Command -> (Word8, Put)
putCommand cmd =
  case cmd of
    ClearScreen ci rpt -> (1,  put ci >> putWord6 rpt >> pad 14)
    ClearBorder ci     -> (2,  put ci >> pad 15)
    SetTile t          -> (6,  put t)
    XorTile t          -> (20, put t)
    ScrollColor ci s   -> (24, put ci >> put s >> pad 14)
    ScrollCopy s       -> (28, pad 1  >> put s >> pad 14)
    SetTransparent ci  -> (30, put ci >> pad 15)
    LoadColorsLo t     -> (31, put t)
    LoadColorsHi t     -> (38, put t)

getCommand :: Int -> Get Command
getCommand tag =
  case tag of
    1  -> ClearScreen <$> get <*> getWord6 <* skip 14
    2  -> ClearBorder <$> get <* skip 15
    6  -> SetTile <$> get
    20 -> XorTile <$> get
    24 -> ScrollColor <$> get <*> get <* skip 14
    28 -> ScrollCopy  <$> (skip 1 *> get <* skip 14)
    30 -> SetTransparent <$> get <* skip 15
    31 -> LoadColorsLo <$> get
    38 -> LoadColorsHi <$> get
    _  -> fail "unknown command"

instance Binary ColorIndex where
  put (ColorIndex i) = putWord4 i
  get =
    do n <- getWord6
       guard (n < 16)
       pure (ColorIndex n)

instance Binary Tile where
  put Tile { .. } =
   do put bg
      put fg
      putWord6 (clamp 0 screenGridHeight row)
      putWord6 (clamp 0 screenGridWidth  col)
      putExactly tileHeight 0 (map word6 (BS.unpack bitmap))

  get =
   do bg   <- get
      fg   <- get
      row  <- getWord6
      guard (row < screenGridHeight)
      col  <- getWord6
      guard (col < screenGridWidth)
      b    <- replicateM tileHeight getWord8
      pure Tile { bitmap = BS.pack (map word6 b), .. }

instance Binary Scroll where
  put Scroll { .. } = putScroll tileWidth hor >> putScroll tileHeight ver
  get =
   do hor <- getScroll tileWidth
      ver <- getScroll tileHeight
      pure Scroll { .. }

putScroll :: Int -> ScrollAmt -> Put
putScroll lim ScrollAmt { .. } = putWord8 ((upper `shiftL` 4) .|. lower)
    where
    upper = case scroll of
              None -> 0
              Next -> 1
              Prev -> 2
    lower = fromIntegral (clamp 0 lim offset)


getScroll :: Int -> Get ScrollAmt
getScroll lim =
  do b <- getWord8
     scroll <- case (b `shiftR` 4) .&. 0x3 of
                 0 -> pure None
                 1 -> pure Next
                 2 -> pure Prev
                 _ -> fail "invalid scroll"
     let offset = fromIntegral (word4 b)
     guard (offset < lim)
     pure ScrollAmt { .. }

instance Binary ColorTable where
  put (ColorTable t) = putExactly 8 (ColorRGB 0 0 0) (V.toList t)
  get = ColorTable . V.fromList <$> replicateM 8 get

instance Binary ColorRGB where
  put rgb =
   do putWord8 ((r `shiftL` 2) .|. gu)
      putWord8 ((gl `shiftL` 4) .|. b)
    where
    norm c = word4 (fromIntegral (clamp 0 256 c))
    r   = norm (red   rgb)
    g   = norm (green rgb)
    gu  = g `shiftR` 2
    gl  = g .&. 0x3
    b   = norm (blue  rgb)

  get =
    do b1 <- getWord8
       b2 <- getWord8
       let red    = fromIntegral ((b1 `shiftR` 2) .&. 0x0F) * 17
           green  = fromIntegral (((b1 .&. 0x3) `shiftL` 2) .|.
                                 ((b2 `shiftL` 4) .&. 0x3)) * 17
           blue   = fromIntegral (b2 .&. 0x0F) * 17
       pure ColorRGB { .. }




