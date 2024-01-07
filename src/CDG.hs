module CDG
  ( Command(..)
  , ColorRGB(..)
  , ColorIndex(..), xorColorIndex, zeroColorIndex
  , ColorTable(..), ansiColors, getColor, setColorLo, setColorHi
  , Tile(..), Sprite, fillSprite, tileSprite, xorSprite, spriteColorAt
  , Scroll(..)
  , ScrollAmt(..)
  ) where

import Control.Monad(guard,replicateM_, replicateM,when)
import Data.Binary
import Data.ByteString(ByteString)
import Data.ByteString qualified as BS
import Data.Vector(Vector)
import Data.Vector qualified as V
import Numeric(showHex)
import Data.Bits

import CDG.Constants

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

showByte :: Int -> ShowS
showByte x
  | x < 0     = error "negative byte"
  | x < 16    = ('0' :) . showHex x
  | otherwise = showHex x

instance Show ColorRGB where
  showsPrec _ ColorRGB { .. } =
    ("0x" ++) . showByte red . showByte green . showByte blue

newtype ColorIndex  = ColorIndex Int
  deriving Eq

instance Show ColorIndex where
  showsPrec p (ColorIndex i) = showsPrec p i

xorColorIndex :: ColorIndex -> ColorIndex -> ColorIndex
xorColorIndex (ColorIndex x) (ColorIndex y) = ColorIndex (x `xor` y)

zeroColorIndex :: ColorIndex
zeroColorIndex = ColorIndex 0

--------------------------------------------------------------------------------
newtype ColorTable  = ColorTable (Vector ColorRGB)

instance Show ColorTable where
  show (ColorTable t) =
    unlines $
      [ sep ++ " " ++ show c
      | (sep,c) <- zip ("[" : repeat ",") (V.toList t)
      ]
      ++ [ "]" ]

ansiColors :: ColorTable
ansiColors = ColorTable (V.fromList colors)
  where
  colors =
    [ ColorRGB 0x00 0x00 0x00
    , ColorRGB 0x80 0x00 0x00
    , ColorRGB 0x00 0x80 0x00
    , ColorRGB 0x80 0x80 0x00
    , ColorRGB 0x00 0x00 0x80
    , ColorRGB 0x80 0x00 0x80
    , ColorRGB 0x00 0x80 0x80
    , ColorRGB 0xc0 0xc0 0xc0

    , ColorRGB 0x80 0x80 0x80
    , ColorRGB 0xFF 0x00 0x00
    , ColorRGB 0x00 0xFF 0x00
    , ColorRGB 0xFF 0xFF 0x00
    , ColorRGB 0x00 0x00 0xFF
    , ColorRGB 0x0FF 0x00 0xFF
    , ColorRGB 0x00 0xFF 0xFF
    , ColorRGB 0xFF 0xFF 0xFF
    ]


getColor :: ColorIndex -> ColorTable -> ColorRGB
getColor (ColorIndex i) (ColorTable t) =
  case t V.!? i of
    Just c  -> c
    Nothing ->
      error $ unlines
                [ "Color index out of bounds: " ++ show i
                , show (ColorTable t)
                ]

setColorLo :: ColorTable -> ColorTable -> ColorTable
setColorLo new (ColorTable old) = ColorTable (V.imap upd old)
  where
  upd i v
    | i < 8     = getColor (ColorIndex i) new
    | otherwise = v

setColorHi :: ColorTable -> ColorTable -> ColorTable
setColorHi new (ColorTable old) = ColorTable (V.imap upd old)
  where
  upd i v
    | i >= 8    = getColor (ColorIndex (i-8)) new
    | otherwise = v

--------------------------------------------------------------------------------

data Tile = Tile
  { fg     :: !ColorIndex
  , bg     :: !ColorIndex
  , row    :: !Int
  , col    :: !Int
  , bitmap :: !ByteString     -- ^ 12 rows, 6 bit/columns
  }

instance Show Tile where
  show Tile { .. } =
    unlines $
      ("{ fg=" ++ show fg ++
        ", bg=" ++ show bg ++
        ", xy=" ++ show (col,row) ++ "}"
      ) :
      [ [ if b `testBit` i then '*' else '_' | i <- reverse [ 0 .. 5 ] ]
      | b <- BS.unpack bitmap
      ]

type Sprite = Vector ColorIndex

fillSprite :: ColorIndex -> Sprite
fillSprite = V.replicate (tileWidth * tileHeight)

tileSprite :: Tile -> Sprite
tileSprite tile =
  V.fromList
    [ if b `testBit` i then fg tile else bg tile
    | b <- BS.unpack (bitmap tile)
    , i <- reverse (take tileWidth [ 0 .. ])
    ]

xorSprite :: Sprite -> Sprite -> Sprite
xorSprite = V.zipWith xorColorIndex

spriteColorAt :: Int -> Int -> Sprite -> ColorIndex
spriteColorAt row col s =
  case s V.!? (row * tileWidth + col) of
    Just i  -> i
    Nothing ->
      error ("sprite index out of bounds: " ++ show row ++ ", " ++ show col)


data ScrollAmt = ScrollAmt
  { scroll :: !Int      -- ^ scroll this many tiles
  , offset :: !Int
  } deriving Show

data Scroll = Scroll
  { hor   :: !ScrollAmt
  , ver   :: !ScrollAmt
  } deriving Show




--------------------------------------------------------------------------------
-- Serialization

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
    ScrollColor ci s   -> (20, put ci >> put s >> pad 14)
    ScrollCopy s       -> (24, pad 1  >> put s >> pad 14)
    SetTransparent ci  -> (28, put ci >> pad 15)
    LoadColorsLo t     -> (30, put t)
    LoadColorsHi t     -> (31, put t)
    XorTile t          -> (38, put t)

getCommand :: Int -> Get Command
getCommand tag =
  case tag of
    1  -> ClearScreen <$> get <*> getWord6 <* skip 14
    2  -> ClearBorder <$> get <* skip 15
    6  -> SetTile <$> get
    20 -> ScrollColor <$> get <*> get <* skip 14
    24 -> ScrollCopy  <$> (skip 1 *> get <* skip 14)
    28 -> SetTransparent <$> get <* skip 15
    30 -> LoadColorsLo <$> get
    31 -> LoadColorsHi <$> get
    38 -> XorTile <$> get
    _  -> fail "unknown command"

instance Binary ColorIndex where
  put (ColorIndex i) = putWord4 i
  get =
    do n <- getWord6
       when (n >= 16) (error "MALFORMED COLOR INDEX")
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
      when (row >= screenGridHeight) (error "MALFORMED TILE")
      col  <- getWord6
      when (col >= screenGridWidth) (error "MALFORMED TILE 2")
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
    upper = case compare scroll 0 of
              EQ -> 0
              GT -> 1
              LT -> 2
    lower = fromIntegral (clamp 0 lim offset)


getScroll :: Int -> Get ScrollAmt
getScroll lim =
  do b <- getWord8
     scroll <- case (b `shiftR` 4) .&. 0x3 of
                 0 -> pure 0
                 1 -> pure 1
                 2 -> pure (-1)
                 _ -> error "invalid scroll"
     let offset = fromIntegral (word4 b)
     when (offset >= lim) (error "invalid scroll 2")
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




