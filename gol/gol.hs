import Data.List (splitAt)
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
defaultBoard = parseBoard s
    where s = ["                ",
               "    O           ",
               "  O O           ",
               "   OO           ",
               "                ",
               "                ",
               "                "]

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
             | Step Int
             | Play Int
             | Place Int Int
             | Toggle Int Int
             | Resize Int Int

process' :: [String] -> Command
process' [c, xs, ys] | c == "p" || c == "place"  = f Place  (readMaybe xs) (readMaybe ys)
                     | c == "t" || c == "toggle" = f Toggle (readMaybe xs) (readMaybe ys)
                     | c == "r" || c == "resize" = f Resize (readMaybe xs) (readMaybe ys)
    where f g (Just x) (Just y) = g x y
          f _ _        _        = Invalid
process' [c, xs] | c == "s" || c == "step" = f Step (readMaybe xs)
                 | c == "p" || c == "play" = f Play (readMaybe xs)
    where f g (Just n) = g n
          f _ _        = Invalid
process' _  = Invalid

process :: String -> Command
process s | s' == "q" || s' == "quit" = Quit
          | s' == "n" || s' == "next" || s' == "" = Next
          | s' == "h" || s' == "help" = Help
          | otherwise = process' $ map trim $ splitOn " " $ trim s'
    where s' = trim s

clearScreenStr :: String
clearScreenStr = "\ESC[2J"

helpStr :: String
helpStr = "=-=-= Help =-=-=\n\
          \quit      : Quit Game of Life\n\
          \next      : Next iteraton\n\
          \help      : Print this message\n\
          \step n    : Calculate n iteratons, and display the result\n\
          \play n    : Calculate and display n iterations\n\
          \place x y : Create life at the position x,y\n\
          \toggle x t: Toggle the life status of position x,y\n\
          \resize w h: Resize the Game of Life window"

invalidStr :: String
invalidStr = "ERROR: Invalid command format.\n\n" ++ helpStr

playCommand :: Command -> Board -> IO ()
playCommand Quit         _ = return ()
playCommand Next         b = play (next b)
playCommand Invalid      b = do putStrLn invalidStr
                                prompt b
playCommand Help         b = do putStrLn helpStr
                                prompt b
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
               threadDelay $ 100 * millisecond
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
main = do putStrLn clearScreenStr
          play defaultBoard