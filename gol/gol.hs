import Data.List (intersperse, splitAt)
import Data.List.Split (splitOn)
import Text.Read (readMaybe)
import Data.Char (isSpace)
import Control.Concurrent (threadDelay)
import Control.Parallel (par, pseq)
import System.IO (hFlush, stdout)

type Board = [[Life]];
data Life = Dead | Alive
    deriving (Eq)

instance Show Life where
    show Dead = " "
    show Alive = "O"

boardH :: Board -> Int
boardH b = length b

boardW :: Board -> Int
boardW b = length (b !! 0)

toggleLife :: Life -> Life
toggleLife Alive = Dead
toggleLife Dead  = Alive

resizeList :: Int -> [a] -> [a] -> [a]
resizeList 0 _  _          = []
resizeList n [] []         = error "resizeList"
resizeList n (x:xs) []     = x : resizeList (n-1) xs []
resizeList n (x:xs) (y:ys) = x : resizeList (n-1) xs ys
resizeList n []     (y:ys) = y : resizeList (n-1) [] ys

resizeBoard :: Int -> Int -> Board -> Board
resizeBoard w 0 _       = []
resizeBoard w h (row:b) = resizeList w row (repeat Dead) : resizeBoard w (h-1) b
resizeBoard w h []      = replicate w Dead               : resizeBoard w (h-1) []

getLife :: Int -> Int -> Board -> Life
getLife x y b = (b !! y) !! x

setLife :: Int -> Int -> Board -> Life -> Board
setLife x 0 (row:b) l = (lrow ++ [l] ++ rrow) : b
    where (lrow, _:rrow) = splitAt x row
setLife x y (row:b) l = row : setLife x (y-1) b l

showBoard :: Board -> String
showBoard b = l ++ "\n" ++ showBoard' b ++ l
    where l = "+" ++ replicate (boardW b) '-' ++ "+"

showBoard' :: Board -> String
showBoard' []     = ""
showBoard' (b:bs) = "|" ++ concat (map show b) ++ "|\n" ++ showBoard' bs

parseLine :: String -> [Life]
parseLine = map f
    where f ' ' = Dead
          f 'O' = Alive

parseBoard :: [String] -> Board
parseBoard = map parseLine

defaultBoard :: Board
defaultBoard = load "glider"

presets :: [(String, String)]
presets = [("blinker", "A simple oscillator of period 2"),
           ("toad", "Another simple oscillator of period 2"),
           ("beacon", "Yet another simple oscillator of period 2"),
           ("pulsar", "A large oscillator of period 3"),
           ("pentadecathlon", "A medium sized oscillator of period 15"),
           ("glider", "A small spaceship that travels diagonally"),
           ("lwss", "Lightweight spaceship that travels horizontally"),
           ("r-pent", "A small seed that grows to live for a long time")]

-- Example interesting Game of Life boards
load :: String -> Board
load s = maybe (error "load: Invalid board ID") id $ loadMaybe s

loadMaybe :: String -> Maybe Board
loadMaybe "blinker" = Just $ parseBoard s
    where s = ["     ",
               "  O  ",
               "  O  ",
               "  O  ",
               "     "]

loadMaybe "toad" = Just $ parseBoard s
    where s = ["      ",
               "      ",
               "  OOO ",
               " OOO  ",
               "      ",
               "      "]

loadMaybe "beacon" = Just $ parseBoard s
    where s = ["      ",
               " OO   ",
               " OO   ",
               "   OO ",
               "   OO ",
               "      "]

-- Period 3
loadMaybe "pulsar" = Just $ parseBoard s
    where s = ["                 ",
               "                 ",
               "    OOO   OOO    ",
               "                 ",
               "  O    O O    O  ",
               "  O    O O    O  ",
               "  O    O O    O  ",
               "    OOO   OOO    ",
               "                 ",
               "    OOO   OOO    ",
               "  O    O O    O  ",
               "  O    O O    O  ",
               "  O    O O    O  ",
               "                 ",
               "    OOO   OOO    ",
               "                 ",
               "                 "]

-- Period 15
loadMaybe "pentadecathlon" = Just $ parseBoard s
    where s = ["           ",
               "           ",
               "           ",
               "           ",
               "           ",
               "    OOO    ",
               "    O O    ",
               "    OOO    ",
               "    OOO    ",
               "    OOO    ",
               "    OOO    ",
               "    O O    ",
               "    OOO    ",
               "           ",
               "           ",
               "           ",
               "           ",
               "           "]

loadMaybe "glider" = Just $ parseBoard s
    where s = ["                   ",
               "     O             ",
               "   O O             ",
               "    OO             ",
               "                   ",
               "                   ",
               "                   ",
               "                   ",
               "                   "]

loadMaybe "lwss" = Just $ parseBoard s
    where s = ["                   ",
               "  O  O             ",
               "      O            ",
               "  O   O            ",
               "   OOOO            ",
               "                   ",
               "                   "]

loadMaybe "r-pent" = Just $ parseBoard s
    where s = ["                          ",
               "                          ",
               "                          ",
               "                          ",
               "                          ",
               "                          ",
               "                          ",
               "                          ",
               "                          ",
               "                          ",
               "                          ",
               "            OO            ",
               "           OO             ",
               "            O             ",
               "                          ",
               "                          ",
               "                          ",
               "                          ",
               "                          ",
               "                          ",
               "                          ",
               "                          ",
               "                          ",
               "                          ",
               "                          ",
               "                          "]

wrap :: Int -> Int -> Board -> (Int, Int)
wrap x y b = (xr, yr)
    where w = boardW b
          h = boardH b
          xm = x `mod` w
          ym = y `mod` h
          xr = if xm >= 0 then xm else w + xm
          yr = if ym >= 0 then ym else h + ym

neighbours :: Int -> Int -> Board -> [Life]
neighbours x y b = map f' $ map f [(x + x', y + y') | x' <- [-1..1], y' <- [-1..1], x' /= 0 || y' /= 0]
    where f  (x, y) = wrap x y b
          f' (x, y) = getLife x y b

countNeighbours :: Int -> Int -> Board -> Int
countNeighbours x y b = length $ filter (==Alive) $ neighbours x y b

next :: Board -> Board
next b = map (\a -> map f' a) $ map (\a -> map f a) [[(x, y) | x <- [0..w-1]] | y <- [0..h-1]]
    where w = boardW b
          h = boardH b
          f  (x, y) = ((b !! y) !! x, countNeighbours x y b)
          f' (Alive, n) | n < 2     = Dead  -- Under-population
                        | n <= 3    = Alive -- Just right
                        | otherwise = Dead  -- Over-population
          f' (Dead , n) | n == 3    = Alive -- Procreation
                        | otherwise = Dead  -- No lovers :'(

nextN :: Int -> Board -> Board
nextN 0 b = b
nextN n b = nextN (n-1) (next b)

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

data Command = Quit
             | Next
             | Invalid
             | Help
             | Presets
             | Load String
             | Step Int
             | Play Int
             | Place Int Int
             | Toggle Int Int
             | Resize Int Int

process' :: [String] -> Command
process' [c, a0, a1] |             c == "place"  = f Place  (readMaybe a0) (readMaybe a1)
                     | c == "t" || c == "toggle" = f Toggle (readMaybe a0) (readMaybe a1)
                     | c == "r" || c == "resize" = f Resize (readMaybe a0) (readMaybe a1)
    where f g (Just x) (Just y) = g x y
          f _ _        _        = Invalid
process' [c, a0] | c == "s" || c == "step" = f Step (readMaybe a0)
                 | c == "p" || c == "play" = f Play (readMaybe a0)
                 | c == "l" || c == "load" = Load a0
    where f g (Just n) = g n
          f _ _        = Invalid
process' _  = Invalid

process :: String -> Command
process s | s' == "q" || s' == "quit"             = Quit
          | s' == "n" || s' == "next" || s' == "" = Next
          | s' == "h" || s' == "help"             = Help
          |              s' == "presets"          = Presets
          | otherwise = process' $ map trim $ splitOn " " $ trim s'
    where s' = trim s

helpStr :: String
helpStr = "=-=-= Help =-=-=\n\
          \quit      : Quit Game of Life\n\
          \next      : Next iteraton\n\
          \help      : Print this message\n\
          \load s    : Loads the preset Game of Life, 's'\n\
          \presets   : (TODO) Prints a list of presets\n\
          \step n    : Calculate n iteratons, and display the result\n\
          \play n    : Calculate and display n iterations\n\
          \place x y : Create life at the position x,y\n\
          \toggle x t: Toggle the life status of position x,y\n\
          \resize w h: Resize the Game of Life window"

pad :: String -> Int -> Char -> String
pad s     0 _ = s
pad []    n x = x : pad [] (n-1) x
pad (c:s) n x = c : pad s  (n-1) x

presetsStr :: String
presetsStr = "=-=-= Presets =-=-=\n" ++ (concat $ intersperse "\n" descs)
    where maxl  = maximum [length x | (x, _) <- presets]
          descs = [pad s maxl ' ' ++ ": " ++ desc | (s, desc) <- presets]

invalidStr :: String
invalidStr = "ERROR: Invalid command format.\n\n" ++ helpStr

invalidLoad :: String -> Board -> IO ()
invalidLoad s b = do putStrLn $ "ERROR: '" ++ s ++ "' is an invalid preset name.\nFor a list of presets, type 'presets'"
                     prompt b

validLoad :: String -> Board -> IO ()
validLoad s b = do putStrLn $ "Loaded '" ++ s ++ "'."
                   play b

-- Executes a command
playCommand :: Command -> Board -> IO ()
playCommand Quit         _ = return ()
playCommand Next         b = play (next b)
playCommand Invalid      b = do putStrLn invalidStr
                                prompt b
playCommand Help         b = do putStrLn helpStr
                                prompt b
playCommand Presets      b = do putStrLn presetsStr
                                prompt b
playCommand (Load s)     b = maybe (invalidLoad s b) (validLoad s) (loadMaybe s)
playCommand (Step n)     b = play $ nextN n b
playCommand (Play n)     b = playN n (next b)
playCommand (Place  x y) b = play $ setLife x y b Alive
playCommand (Toggle x y) b = play $ setLife x y b (toggleLife (getLife x y b))
playCommand (Resize w h) b = play $ resizeBoard w h b

-- Millisecond in microseconds
millisecond :: Int
millisecond = 1000

playN :: Int -> Board -> IO ()
playN 1 b = play b
playN n b = do putStrLn $ showBoard b
               b' <- pseq 0 (return (next b))
               threadDelay $ 150 * millisecond
               playN (n-1) b'

prompt :: Board -> IO ()
prompt b = do putStr "> "
              hFlush stdout
              s <- getLine
              playCommand (process s) b

play :: Board -> IO ()
play b = do putStrLn $ showBoard b
            prompt b

main :: IO ()
main = play defaultBoard
