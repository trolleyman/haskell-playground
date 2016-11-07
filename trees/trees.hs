import System.Random
import System.IO.Unsafe
import Debug.Trace

data BinTree a = Empty
               | Branch a (BinTree a) (BinTree a)
    deriving (Show, Eq)

m = Branch 19
        (Branch 20 Empty Empty)
        (Branch 21 Empty Empty)

{-
      5   1
     /   /
t = 4   2
     \ / \
      3   7
       \
        9
-}

t = Branch 4
    (leaf 5)
    (Branch 3
        (Branch 2
            (leaf 1)
            (leaf 7))
        (leaf 9))
    where leaf x = Branch x Empty Empty

data Direction = DirLeft | DirRight
    deriving (Show, Eq)
type Path = [Direction]

-- rtree 1232 0.1 0 100
rtree :: Int -> Float -> Int -> Int -> BinTree Int
rtree seed p lo hi =
    let (t, _) = rtreegen (mkStdGen seed) p lo hi in
    t

rtreegen :: StdGen -> Float -> Int -> Int -> (BinTree Int, StdGen)
rtreegen gen0 p lo hi =
    let (f, gen1) = random gen0 :: (Float, StdGen) in
    if f < p then
        let (x, gen2) = randomR (lo, hi) gen1 :: (Int, StdGen) in
        let (l, gen3) = rtreegen gen2 p lo hi in
        let (r, gen4) = rtreegen gen3 p lo hi in
        (Branch x l r, gen4)
    else
        (Empty, gen1)

rtreeh :: Int -> Int -> Int -> Int -> BinTree Int
rtreeh seed height lo hi =
    let (t, _) = (rtreehgen (mkStdGen seed) height lo hi) in
    t

rtreehgen :: StdGen -> Int -> Int -> Int -> (BinTree Int, StdGen)
rtreehgen gen0 height lo hi =
    if height > 0 then
        let (x, gen1) = randomR (lo, hi) gen0 :: (Int, StdGen) in
        let (l, gen2) = rtreehgen gen1 (height - 1) lo hi in
        let (r, gen3) = rtreehgen gen2 (height - 1) lo hi in
        (Branch x l r, gen3)
    else
        (Empty, gen0)

path :: BinTree a -> Path -> Maybe a
path Empty          _             = Nothing
path (Branch x _ _) []            = Just x
path (Branch _ l _) (DirLeft :ps) = path l ps
path (Branch _ _ r) (DirRight:ps) = path r ps

paths :: BinTree a -> [[Direction]]
paths Empty = []
paths (Branch _ Empty Empty) = [[]]
paths (Branch _ l     Empty) = [DirLeft :p | p <- paths l]
paths (Branch _ Empty r    ) = [DirRight:p | p <- paths r]
paths (Branch _ l     r    ) = [DirLeft :p | p <- paths l]
                            ++ [DirRight:p | p <- paths r]

leaves :: BinTree a -> Int
leaves Empty = 0
leaves (Branch _ Empty Empty) = 1
leaves (Branch _ l     r    ) = leaves l + leaves r

size :: BinTree a -> Int
size Empty          = 0
size (Branch _ l r) = size l + size r + 1

height :: BinTree a -> Int
height Empty          = 0
height (Branch _ l r) = max (height l) (height r) + 1
