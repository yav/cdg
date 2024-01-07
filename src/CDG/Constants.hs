module CDG.Constants where

-- | In bytes
packetSize :: Int
packetSize = 24

-- | In pixels
tileWidth :: Int
tileWidth = 6

-- | In pixels
tileHeight :: Int
tileHeight = 12

hBorder :: Int
hBorder = tileWidth `quot` 2

vBorder :: Int
vBorder = tileHeight `quot` 2

-- | In tiles
screenGridWidth :: Int
screenGridWidth = 50

-- | In tiles
screenGridHeight :: Int
screenGridHeight = 18

screenGridLastRow :: Int
screenGridLastRow = screenGridHeight - 1

screenGridLastColumn :: Int
screenGridLastColumn = screenGridWidth - 1

-- | In pixels
screenVisibleWidth :: Int
screenVisibleWidth = tileWidth * screenGridLastColumn

-- | In pixels
screenVisibleHeight :: Int
screenVisibleHeight = tileHeight * screenGridLastRow

packetsPerSec :: Int
packetsPerSec = 300

