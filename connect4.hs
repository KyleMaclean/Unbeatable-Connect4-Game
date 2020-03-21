import Data.List
import Data.Char
import Data.Ord

rows = 6
cols = 7
win = 4
depth = 4

type Board = [Row]
type Row = [Player]
data Player = O | B | X deriving (Ord, Eq, Show)

main :: IO ()
main = nextPly O blankBoard

blankBoard :: Board 
blankBoard = replicate rows (replicate cols B)

nextPly :: Player -> Board -> IO ()
nextPly p b = do putStr "\ESC[2J\ESC[1;1H" -- comment out if running on Windows
                 showBoard b
                 if anyHasWon b then putStrLn (showPlayer (winner b) : " has won.")
                 else if isFull b then putStrLn "Draw"
                 else if p == O then (nextPly X (idealPath O b))
                 else do
                        c <- getCheckCol "O was placed by AI." b
                        nextPly O (ply X b ((lowestRow b c (rows-1)),c))   

getCheckCol :: String -> Board -> IO Int
getCheckCol s b = do putStrLn (s ++ " Enter column for X: ")
                     c <- getLine
                     if c /= [] && all isDigit c && (read c::Int) >= 0 && (read c::Int) < cols && any isBlank ((transpose b) !! (read c::Int)) then 
                       return (read c)
                     else getCheckCol "Column is invalid or full." b

showBoard :: Board -> IO ()
showBoard b =
             putStrLn (unlines (map showRow b ++ [line] ++ [nums]))
             where
                  showRow = map showPlayer
                  line = replicate cols '-'
                  nums = take cols ['0'..]
                  
showPlayer :: Player -> Char
showPlayer O = 'O'
showPlayer B = '.'
showPlayer X = 'X'

data Tree a = Node a [Tree a] deriving Show

tree :: Player -> Board -> Tree Board
tree p b = Node b [tree (changePlayer p) neighbour | neighbour <- neighbourhood p b]

prune :: Int -> Tree a -> Tree a
prune 0 (Node x _) = Node x []
prune d (Node x ts) = Node x [prune (d-1) t | t <- ts]

labelTree :: Tree Board -> Tree (Board,Player,Int)
labelTree (Node b []) = Node (b,winner b,0) []
labelTree (Node b ts) = Node (b, wp, rd + 1) ts'
                          where
                               ts' = map labelTree ts
                               (wp,rd) = byOrdering (whosePly b) [(player,rd) | Node (_, player, rd) _ <- ts']

byOrdering :: Player -> [(Player, Int)] -> (Player, Int)
byOrdering p = (if p == O then minimumBy else maximumBy) (dependentOrdering p)

dependentOrdering :: Player -> (Player, Int) -> (Player, Int) -> Ordering
dependentOrdering p (p1, rd1) (p2, rd2) = compare p1 p2 <> case (p, p1) of
                                                                (_, O) -> compare rd1 rd2
                                                                (_, X) -> compare rd2 rd1
                                                                (_, B) -> EQ

whosePly :: Board -> Player
whosePly b = if os <= xs then O else X
             where
                   os = length (filter (== O) flat)
                   xs = length (filter (== X) flat)
                   flat = concat b

idealPath :: Player -> Board -> Board
idealPath p b = fst (minimumBy (comparing snd) (nonLosingPaths p b))

nonLosingPaths :: Player -> Board -> [(Board,Int)]
nonLosingPaths p b = [(b',rd') | Node (b',p',rd') ts' <- ts, p' == this, rd == rd' + 1]
                     where
                          Node (_,this,rd) ts = labelTree (prune depth (tree p b)) 

neighbourhood :: Player -> Board -> [Board]
neighbourhood p b | anyHasWon b || isFull b = []
                  | otherwise = [ply p b ((lowestRow b c (rows-1)),c) | c <- [0..cols-1], any isBlank ((transpose b) !! c)]

ply :: Player -> Board -> (Int,Int) -> Board
ply p b (r,c) = take r b ++ newRow ++ drop (r+1) b
                where
                     newRow = [(take c (b !! r) ++ [p] ++ drop (c+1) (b !! r))]

lowestRow :: Board -> Int -> Int -> Int
lowestRow b c r | isBlank (transpose b !! c !! r) = r
                | r < 0 = -1
                | otherwise = lowestRow b c (r-1)

allDiagonals :: Board -> [Row]
allDiagonals b =
                 take vertical (belowDiagonals (tail b))
              ++ take horizontal (belowDiagonals transposed)
              ++ take vertical (belowDiagonals (tail (reverse b)))
              ++ take horizontal (belowDiagonals (transpose (reverse b)))
                 where
                   transposed = transpose b
                   vertical = rows - win
                   horizontal = cols - win + 1

belowDiagonals :: Board -> [Row]
belowDiagonals [] = []
belowDiagonals b@(_:rs) = mainDiagonal b : belowDiagonals rs

mainDiagonal :: Board -> Row
mainDiagonal [] = []
mainDiagonal ([]:rs) = []
mainDiagonal ((p:r):rs) = p : mainDiagonal (map tail rs)

winner :: Board -> Player
winner b | hasWon X b = X
         | hasWon O b = O
         | otherwise = B

isFull :: Board -> Bool
isFull = all (/=B) . concat

anyHasWon :: Board -> Bool
anyHasWon b = hasWon X b || hasWon O b

hasWon :: Player -> Board -> Bool
hasWon p b = connections p b
          || connections p (transpose b)
          || connections p (allDiagonals b)

connections :: Player -> [Row] -> Bool
connections _ [] = False
connections p (r:b) = connection p r || connections p b

connection :: Player -> [Player] -> Bool
connection _ [] = False
connection p ps = winningLine == candidateLine || connection p (tail ps)
                  where 
                       winningLine = replicate win p
                       candidateLine = take win ps

changePlayer :: Player -> Player
changePlayer O = X
changePlayer X = O

isBlank :: Player -> Bool
isBlank p = p == B

showBoards :: [Board] -> IO ()
showBoards [] = putStrLn ""
showBoards (b:bs) =
                   do
                     showBoard b
                     showBoards bs
