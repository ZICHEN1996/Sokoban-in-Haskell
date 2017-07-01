{-# LANGUAGE OverloadedStrings #-}
import CodeWorld

-- Make sure that you edit all pieces of code marked as "undefined" and "FIXME"

-- Lists

data List a = Empty | Entry a (List a)
    deriving (Eq)

mapList :: (a -> b) -> List a -> List b
mapList _ Empty = Empty
mapList f (Entry c cs) = Entry (f c) (mapList f cs)

combine :: List Picture -> Picture
combine Empty = blank
combine (Entry p ps) = p & combine ps

-- Coordinates

data Coord = C Integer Integer
    deriving (Eq)

data Direction = R | U | L | D

eqCoord :: Coord -> Coord -> Bool
eqCoord a b
    | a == b = True
    | otherwise = False

adjacentCoord :: Direction -> Coord -> Coord
adjacentCoord R (C x y) = C (x+1) y
adjacentCoord U (C x y) = C  x   (y+1)
adjacentCoord L (C x y) = C (x-1) y
adjacentCoord D (C x y) = C  x   (y-1)


-- `moveFromTo` updates the coordinates (of boxes) 
moveFromTo :: Coord -> Coord -> (Coord -> Coord)
moveFromTo from to box | from `eqCoord` box = to
                       | otherwise          = box


-- The maze

data Tile = Wall | Ground | Storage | Box | Blank
    deriving (Eq)
       
maze :: Coord -> Tile 
maze (C x y)
    | abs x > 4  ||  y < -5  ||  y > 4   = Blank
    | abs x == 4 ||  y == -5 ||  y == 4  = Wall
    | x == 3 && y >= -2          = Wall
    | y == 3 && x <= -2          = Wall
    | y == 1 && x <= -2          = Wall
    | y == (-1) && (x == (-1) || x == (-2)) = Wall
    | y == (-2) && x == (-2)     = Wall
    | (x == 3 && y == (-3)) || (x == 2 && y == 1) || (x == 1 && y == (-2)) || (x == 1 && y == (-4)) || (x == (-3) && y == (-1)) || (x == (-3) && y == 2) = Storage
    | (x == 2 && y == (-3)) || (x == 1 && y == (-3)) || (x == 1 && y == (-1)) || (x == 1 && y == 1) || (x == (-1) && y == 2) || (x == (-3) && y == (-3)) = Box
    | otherwise                  = Ground 
 
 
mazeWithoutBoxes :: Coord -> Tile
mazeWithoutBoxes c = case maze c of
    Box -> Ground
    _ -> maze c

mazeWithBoxes :: List Coord -> Coord -> Tile
mazeWithBoxes (Entry c cs) b
    | b `eqCoord` c = Box
    | otherwise = mazeWithBoxes cs b
mazeWithBoxes Empty b = mazeWithoutBoxes b
-- The state

data State = St Coord Direction (List Coord)

initialBoxes :: List Coord
initialBoxes = go (-10) (-10)
    where
        go :: Integer -> Integer -> List Coord
        go 10 10 = Empty
        go a 10 = go (a + 1) (-10)
        go a b = case maze (C a b) of
            Box -> Entry (C a b) (go a (b + 1))
            _   -> go a (b + 1)


initialState :: State
initialState = St (C (-2) 2) R initialBoxes

-- Event handling

handleEvent :: Event -> State -> State
handleEvent (KeyPress key) (St a b c)
    | gameWon c = (St a b c)
    | key == "Right" = tryMove (St a b c) R
    | key == "Up"    = tryMove (St a b c) U
    | key == "Left"  = tryMove (St a b c) L
    | key == "Down"  = tryMove (St a b c) D
handleEvent _ (St a b c) = (St a b c)

tryMove :: State -> Direction -> State
tryMove (St from _ boxes) d
    | ((mazeWithBoxes boxes to) == Ground) || ((mazeWithBoxes boxes to) == Storage) = St to d boxes
    | ((mazeWithBoxes boxes to) == Box) && (((mazeWithBoxes boxes (adjacentCoord d to)) == Ground ) || ((mazeWithBoxes boxes (adjacentCoord d to)) == Storage )) = St to d movedBoxes
    | otherwise = St from d boxes
    where
    to :: Coord
    to = adjacentCoord d from
    movedBoxes :: List Coord
    movedBoxes = mapList (moveFromTo to (adjacentCoord d to)) boxes
-- Drawing

wall, ground, storage, box :: Picture
wall =    colored (grey 0.4) (solidRectangle 1 1)
ground =  colored yellow     (solidRectangle 1 1)
storage = colored white      (solidCircle    0.3) & ground
box =     colored brown      (solidRectangle 1 1)

drawTile :: Tile -> Picture
drawTile Wall    = wall
drawTile Ground  = ground
drawTile Storage = storage
drawTile Box     = box
drawTile Blank   = blank

pictureOfMaze :: Picture
pictureOfMaze = draw21times (\r -> draw21times (\c -> drawTileAt (C r c)))

draw21times :: (Integer -> Picture) -> Picture
draw21times something = go (-10)
    where
      go :: Integer -> Picture
      go 11 = blank
      go n  = something n & go (n+1)

drawTileAt :: Coord -> Picture
drawTileAt c = atCoord c (drawTile (mazeWithoutBoxes c)) -- FIXME


atCoord :: Coord -> Picture -> Picture
atCoord (C x y) pic = translated (fromIntegral x) (fromIntegral y) pic


player :: Direction -> Picture
player R = translated 0 0.3 cranium
         & path [(0,0),(0.3,0.05)] 
         & path [(0,0),(0.3,-0.05)] 
         & path [(0,-0.2),(0,0.1)] 
         & path [(0,-0.2),(0.1,-0.5)]
         & path [(0,-0.2),(-0.1,-0.5)]
  where cranium = circle 0.18
                & sector (7/6*pi) (1/6*pi) 0.18
player L = scaled (-1) 1 (player R) -- Cunning!
player U = translated 0 0.3 cranium
         & path [(0,0),(0.3,0.05)] 
         & path [(0,0),(-0.3,0.05)] 
         & path [(0,-0.2),(0,0.1)] 
         & path [(0,-0.2),(0.1,-0.5)]
         & path [(0,-0.2),(-0.1,-0.5)]
    where cranium = solidCircle 0.18
player D = translated 0 0.3 cranium
         & path [(0,0),(0.3,-0.05)] 
         & path [(0,0),(-0.3,-0.05)] 
         & path [(0,-0.2),(0,0.1)] 
         & path [(0,-0.2),(0.1,-0.5)]
         & path [(0,-0.2),(-0.1,-0.5)]
    where cranium = circle 0.18
                & translated   0.06  0.08 (solidCircle 0.04)
                & translated (-0.06) 0.08 (solidCircle 0.04)

pictureOfBoxes :: List Coord -> Picture
pictureOfBoxes lcs = combine (mapList (\c -> atCoord c (drawTile Box)) lcs)



drawState :: State -> Picture
drawState (St c d l) 
    | gameWon l = endScreen & (atCoord c (player d)) & (pictureOfBoxes l) & pictureOfMaze
    | otherwise = (atCoord c (player d)) & (pictureOfBoxes l) & pictureOfMaze -- FIXME



-- The general interaction type

data Interaction world = Interaction
        world
        (Double -> world -> world)
        (Event -> world -> world)
        (world -> Picture)


runInteraction :: Interaction s -> IO ()
runInteraction (Interaction state0 step handle draw)
    = interactionOf state0 step handle draw
  
  -- The complete interaction

warehouse :: Interaction State
warehouse = Interaction initialState (\_ c -> c) handleEvent drawState

-- Resetable interactions

resetable :: Interaction s -> Interaction s
resetable (Interaction state0 step handle draw)
    = Interaction state0 step handle' draw
    where handle' (KeyPress key) _ | key == "Esc" = state0
          handle' e s = handle e s

-- Start screen

startScreen :: Picture
startScreen = scaled 3 3 (text "Warehouse!")

data SSState world = StartScreen | Running world

withStartScreen :: Interaction s  -> Interaction (SSState s)
withStartScreen (Interaction state0 step handle draw)
    = Interaction state0' step' handle' draw'
    where
      state0' = StartScreen
    
      step' _ StartScreen = StartScreen
      step' t (Running s) = Running (step t s)
    
      handle' (KeyPress key) StartScreen | key == " " = Running state0
      handle' _              StartScreen              = StartScreen
      handle' e              (Running s)              = Running (handle e s)
    
      draw' StartScreen = startScreen
      draw' (Running s) = draw s

-- End screen

endScreen :: Picture
endScreen = scaled 3 3 (text "You won!")

listOfStorage :: List Coord
listOfStorage = go (-10) (-10)
    where
        go :: Integer -> Integer -> List Coord
        go 10 10 = Empty
        go a 10 = go (a + 1) (-10)
        go a b = case maze (C a b) of
            Storage -> Entry (C a b) (go a (b + 1))
            _   -> go a (b + 1)

gameWon :: List Coord -> Bool
gameWon a
    | a == listOfStorage = True
    | otherwise = False


-- The main function

main :: IO ()
main = runInteraction (resetable (withStartScreen warehouse))