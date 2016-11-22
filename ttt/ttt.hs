import System.IO (hFlush, stdout)
import Data.List (transpose, zipWith)
import Data.List.Split (splitOn)
import Text.Read (readMaybe)

data Player = O | B | X
    deriving (Show, Eq, Ord)
type Board = [[Player]]
type Move = (Int, Int, Player)
data Tree m b = Node b [(m, Tree m b)]
    deriving (Show)

sampleBoard :: Board
sampleBoard = [[X,O,X],[O,X,O],[B,B,B]]

genSampleTree :: Player -> Tree Move Board
genSampleTree p = genTree p sampleBoard

nextPlayer :: Player -> Player
nextPlayer O = X
nextPlayer X = O

putBoard :: Board -> IO ()
putBoard b = putStr $ showBoard b

showBoard :: Board -> String
showBoard [] = ""
showBoard [r] = rowSep False n ++ showRow r ++ rowSep False n -- last row
    where n = length r
showBoard (r:b) = rowSep False n ++ showRow r ++ rowSep False n ++ rowSep True n ++ showBoard b
    where n = length r

rowSep :: Bool -> Int -> String
rowSep True  n = concat (replicate (n - 1) "---+") ++ "---\n"
rowSep False n = concat (replicate (n - 1) "   |") ++ "   \n"

showRow :: [Player] -> String
showRow []    = ""
showRow [p]   = " " ++ showPlayer p ++ " \n"
showRow (p:r) = " " ++ showPlayer p ++ " |" ++ showRow r 

showPlayer :: Player -> String
showPlayer B = " "
showPlayer O = "O"
showPlayer X = "X"

newBoard :: Int -> Board
newBoard n = replicate n (replicate n B)

getPlayer :: Board -> Int -> Int -> Player
getPlayer b x y = (b !! y) !! x

setPlayer :: Board -> Int -> Int -> Player -> Board
setPlayer (r:b) x 0 p = setItem x p r : b
    where setItem 0 y (x:xs) = y:xs
          setItem i y (x:xs) = x:setItem (i-1) y xs
setPlayer (r:b) x y p = r : setPlayer b x (y-1) p

-- 'hasWon b O' implies that the game was a draw
hasWon :: Board -> Player -> Bool
hasWon b B | hasWon b X || hasWon b O = False
           | otherwise                = [] == blankSpaces b
hasWon b p = (any id $ map (all (==p))  b)
          || (any id $ map (all (==p)) tb)
          || (all (==p) $ zipWith (!!)  b [0..])
          || (all (==p) $ zipWith (!!) rb [0..])
    where tb = transpose b
          rb = reverse b

won :: Board -> Maybe Player
won b | hasWon b O = Just O
      | hasWon b X = Just X
      | hasWon b B = Just B
      | otherwise  = Nothing

isFinished :: Board -> Bool
isFinished b = hasWon b B || hasWon b O || hasWon b X

availableMoves :: Board -> Player -> [Move]
availableMoves [] p = []
availableMoves b  p | isFinished b = []
                    | otherwise    = [(x, y, p) | (x, y) <- blankSpaces b]

blankSpaces :: Board -> [(Int, Int)]
blankSpaces []    = []
blankSpaces (r:b) = [(x, 0    ) | (x, p) <- zip [0..] r, p == B]
                 ++ [(x, y + 1) | (x, y) <- blankSpaces b]

applyMove :: Board -> Move -> Board
applyMove b (x, y, p) = setPlayer b x y p

genTree :: Player -> Board -> Tree Move Board
genTree p b = Node b nodes
    where moves = [(m, applyMove b m) | m <- availableMoves b p]
          nodes = [(m, tree) | (m, b) <- moves, let tree = genTree (nextPlayer p) b]

height :: Tree m b -> Int
height (Node _ []) = 1
height (Node _ ts) = 1 + maximum [height t | (_, t) <- ts]

size :: Tree m b -> Int
size (Node _ []) = 1
size (Node _ ts) = 1 + sum [size t | (_, t) <- ts]

prune :: Int -> Tree m b -> Tree m b
prune 0 (Node b _)  = Node b []
prune n (Node b ts) = Node b [(m, prune (n-1) t) | (m, t) <- ts]

isValid :: Board -> Move -> Bool
isValid b (x, y, p) = getPlayer b x y == B

parseMove' :: Player -> [Maybe Int] -> Maybe Move
parseMove' p [Just x, Just y] = Just (x, y, p)
parseMove' _ _                = Nothing

parseMove :: Board -> Player -> String -> Maybe Move
parseMove b p s = case parseMove' p (map readMaybe $ splitOn " " s) of
    Just m  -> if isValid b m then Just m else Nothing
    Nothing -> Nothing

prompt :: Board -> Player -> IO Move
prompt b p = do putStr "Player "
                putStr $ show p
                putStr ", it is your turn. Enter your move: "
                hFlush stdout
                line <- getLine
                maybe promptError (\m -> return m) (parseMove b p line)
    where promptError = do putStrLn "Invalid move."
                           prompt b p

playHuman' :: Board -> Player -> IO ()
playHuman' b p = case won b of
    Just B  -> do putBoard b
                  putStrLn "Draw!"
    Just p  -> do putBoard b
                  putStr "Player "
                  putStr $ show p
                  putStrLn " has won!"
    Nothing -> do putBoard b
                  move <- prompt b p
                  let b' = applyMove b move
                  playHuman' b' (nextPlayer p)

playHuman :: Player -> IO ()
playHuman p = playHuman' (newBoard 3) p
