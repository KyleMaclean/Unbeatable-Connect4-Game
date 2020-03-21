> import Data.List
> import Data.Char
> import Data.Ord

PARAMETERS

> rows = 6
> cols = 7
> win = 4
> depth = 4

DEFINITIONS

> type Board = [Row]
> type Row = [Player]

> data Player = O | B | X deriving (Ord, Eq, Show)

USER INTERFACE: INITIALISATION, SHOWING & LOOPING

Player O is passed into `nextPly` as AI starts by default becayse the AI is easily exploitable when it has the disadvantage of starting second. To give the human the starting ply, pass X instead of O into `nextPly`. "blankBoard" is defined according to the board dimension parameters.

> main :: IO ()
> main = nextPly O blankBoard

> blankBoard :: Board 
> blankBoard = replicate rows (replicate cols B)

The game loop, which, for every ply: starts drawing from top left corner of cleared screen, checks for game over, calls AI to search its pruned tree and allows human to input their desired column. The "else" case is only reachable when the player is X (ie., human), with an opportunity to ply.       

> nextPly :: Player -> Board -> IO ()
> nextPly p b = do putStr "\ESC[2J\ESC[1;1H" -- comment out if running on Windows
>                  showBoard b
>                  if anyHasWon b then putStrLn (showPlayer (winner b) : " has won.")
>                  else if isFull b then putStrLn "Draw"
>                  else if p == O then (nextPly X (idealPath O b))
>                  else do
>                         c <- getCheckCol "O was placed by AI." b
>                         nextPly O (ply X b ((lowestRow b c (rows-1)),c))   

Utility for `nextPly` to retrieve and validate the human's desired column. It will prompt the human indefinitely unless a column in range with at least one blank entry is entered.

> getCheckCol :: String -> Board -> IO Int
> getCheckCol s b = do putStrLn (s ++ " Enter column for X: ")
>                      c <- getLine
>                      if c /= [] && all isDigit c && (read c::Int) >= 0 && (read c::Int) < cols && any isBlank ((transpose b) !! (read c::Int)) then 
>                        return (read c)
>                      else getCheckCol "Column is invalid or full." b

Writing the board to the screen.

> showBoard :: Board -> IO ()
> showBoard b =
>              putStrLn (unlines (map showRow b ++ [line] ++ [nums]))
>              where
>                   showRow = map showPlayer
>                   line = replicate cols '-'
>                   nums = take cols ['0'..]
                  
> showPlayer :: Player -> Char
> showPlayer O = 'O'
> showPlayer B = '.'
> showPlayer X = 'X'

TREES AND THEIR UTILITIES

> data Tree a = Node a [Tree a] deriving Show

> tree :: Player -> Board -> Tree Board
> tree p b = Node b [tree (changePlayer p) neighbour | neighbour <- neighbourhood p b]

Children after the desired depth are not considered and thus the base case of `prune` represents such potential children as an empty list. These children are hence considered as leaves. The recursive case generates a list of all children and calls `prune` on each with one less level in the tree to go (by decrementing "d")

> prune :: Int -> Tree a -> Tree a
> prune 0 (Node x _) = Node x []
> prune d (Node x ts) = Node x [prune (d-1) t | t <- ts]

LABELLING

Returns a triple containing the board, the player label, and the "reverse depth" ("rd"), which is defined as the number of boards from a leaf and is calculated by labeling the "reverse depth" of leaves as 0; each successive ancestor recursively increments this value. `byOrdering` is determined by a long case analysis which involves threading through the current player to determine how to compare "rd" values and is explained fruther below.

> labelTree :: Tree Board -> Tree (Board,Player,Int)
> labelTree (Node b []) = Node (b,winner b,0) []
> labelTree (Node b ts) = Node (b, wp, rd + 1) ts'
>                           where
>                                ts' = map labelTree ts
>                                (wp,rd) = byOrdering (whosePly b) [(player,rd) | Node (_, player, rd) _ <- ts']

These two functions compute an Ordering and then use it to choose between `maximumBy` and `minimumBy` according to the objective of the given player. In the minimax paradigm, O is the minimiser and X is the maximiser; accordingly, the ordering of the "reverse depths" ("rd") integers must be deliberately flipped for each player. The board which is best for the given player based on the derived ordering is then extracted from all the boards (also based on which player is extracting) so that the `labelTree` function above can label "winning player" ("wp") and "rd" appropriately.

> byOrdering :: Player -> [(Player, Int)] -> (Player, Int)
> byOrdering p = (if p == O then minimumBy else maximumBy) (dependentOrdering p)

> dependentOrdering :: Player -> (Player, Int) -> (Player, Int) -> Ordering
> dependentOrdering p (p1, rd1) (p2, rd2) = compare p1 p2 <> case (p, p1) of
>                                                                 (_, O) -> compare rd1 rd2
>                                                                 (_, X) -> compare rd2 rd1
>                                                                 (_, B) -> EQ

Returns the player who is due to ply based on the constitution of the board based on the logic that whichever player has less of their pieces on the board is owed the subsequent ply. If the board is empty, the AI should by default ply first, and the AI player is by default assigned player O, thus it is the turn of O on an empty board.

> whosePly :: Board -> Player
> whosePly b = if os <= xs then O else X
>              where
>                    os = length (filter (== O) flat)
>                    xs = length (filter (== X) flat)
>                    flat = concat b

PATH CHOOSING

Selects the board which is closest to a leaf on a winning path, or, the furthest board (requiring the most plies to reach) if all paths end in a loss. The board is extracted from the pair according to its integer label (see `labelTree`).

> idealPath :: Player -> Board -> Board
> idealPath p b = fst (minimumBy (comparing snd) (nonLosingPaths p b))

Keeps only boards which are labeled with "this" player passed as an argument. The named argument "p" is only used to determine the corresponding neighbourhood of boards which can be reached from that player plying, but it is effectively the same the player which is given the name, "this". The predicates in the list comprehension ensure that only the desired player is selected and that boards strictly from each subsequent "reverse depth" are selected. The selections come from the tree which is generated and labeled in the where clause. The function returns a list of doubles instead of triples because it is redundant to store the same player in all of them.

> nonLosingPaths :: Player -> Board -> [(Board,Int)]
> nonLosingPaths p b = [(b',rd') | Node (b',p',rd') ts' <- ts, p' == this, rd == rd' + 1]
>                      where
>                           Node (_,this,rd) ts = labelTree (prune depth (tree p b)) 

SUBSEQUENT BOARDS

Returns a list of all possible ways to place the given player into the given board. The first case returns an empty list if the game is over. The second case generates a list of boards where each has the given player placed in the lowest blank cell of each column.

> neighbourhood :: Player -> Board -> [Board]
> neighbourhood p b | anyHasWon b || isFull b = []
>                   | otherwise = [ply p b ((lowestRow b c (rows-1)),c) | c <- [0..cols-1], any isBlank ((transpose b) !! c)]

Places the given player into the given cell into the given board and return the new board. This is achieved through surgery on the rows and columns to remove the previous entry and insert the new one.

> ply :: Player -> Board -> (Int,Int) -> Board
> ply p b (r,c) = take r b ++ newRow ++ drop (r+1) b
>                 where
>                      newRow = [(take c (b !! r) ++ [p] ++ drop (c+1) (b !! r))]

Returns the bottom-most row in the given column of the given board with a blank cell. Usage: to be called with the maximum-possible rows (thus starting at the lowest row in the board) and decrease this number (thus moving up to the top of the board) until a blank is found. The row value is recursively decreased (to move up the board) at each encounter with a non-blank cell.

> lowestRow :: Board -> Int -> Int -> Int
> lowestRow b c r | isBlank (transpose b !! c !! r) = r
>                 | r < 0 = -1
>                 | otherwise = lowestRow b c (r-1)

DIAGONALS

Returns (left & right) diagonals (above & below) the main (without forgetting the original main) (4 cases) as rows by calculating each list of diagonals through manipulating the given board differntly for each case. Each of the four concatenations handles each case. The first retrieves the "left & below" diagonals, the second retrieves the "left & above" diagonals, the third retrieves the "right & above" diagonals and the fourth retrieves the "right & below" diagonals. "horizontal" is defined with (+ 1) to save us from off-by-one errors related to having / not having the main diagonal of each recursively-smaller board.

> allDiagonals :: Board -> [Row]
> allDiagonals b =
>                  take vertical (belowDiagonals (tail b))
>               ++ take horizontal (belowDiagonals transposed)
>               ++ take vertical (belowDiagonals (tail (reverse b)))
>               ++ take horizontal (belowDiagonals (transpose (reverse b)))
>                  where
>                    transposed = transpose b
>                    vertical = rows - win
>                    horizontal = cols - win + 1

Returns, as rows, the left diagonals below the main left diagonal from the given board. The recursive case names the board so that its main diagonal can be added to the list. The first row is discarded and the resulting board (which is now one row smaller and thus have a new main diagonal) is recurisvely processed.

> belowDiagonals :: Board -> [Row]
> belowDiagonals [] = []
> belowDiagonals b@(_:rs) = mainDiagonal b : belowDiagonals rs

Extracts the main diagonal from a given board. The first base case is matched when the board either: runs out of rows but there are still more columns, or runs out of rows and columns simultaneously (implying that the board is square). The second base case is matched when the board runs out of columns but there are still more rows. The recursive case extracts the first player from the first row, drops the first player from all rows except the first and recursively processes this new board with (with the first row excluded).

> mainDiagonal :: Board -> Row
> mainDiagonal [] = []
> mainDiagonal ([]:rs) = []
> mainDiagonal ((p:r):rs) = p : mainDiagonal (map tail rs)

GAME OVER CHECKING

Wrapper for the hasWon function to return the winning player.

> winner :: Board -> Player
> winner b | hasWon X b = X
>          | hasWon O b = O
>          | otherwise = B

The following two functions determine whether the game is over and are thus called before each ply and in the process of determining neighbourhoods.

> isFull :: Board -> Bool
> isFull = all (/=B) . concat

> anyHasWon :: Board -> Bool
> anyHasWon b = hasWon X b || hasWon O b

The three cases in which a player can win are checked in this order as separate lines in the definition: case 1: no transformation is required to inspect horizontal lines; case 2: transposition is required to represent all columns as rows; case 3: left and right diagonals are transformed into rows using `allDiagonals`

> hasWon :: Player -> Board -> Bool
> hasWon p b = connections p b
>           || connections p (transpose b)
>           || connections p (allDiagonals b)

Looks for a connection using naive string matching. Its type is declared as taking [Row] instead of Board to emphasise that it is used on lists of players which may have been represented vertically, horizontally or diagonally in the actual board.

> connections :: Player -> [Row] -> Bool
> connections _ [] = False
> connections p (r:b) = connection p r || connections p b

Given a player and list of players, looks for a contiguous series of that player in that list.

> connection :: Player -> [Player] -> Bool
> connection _ [] = False
> connection p ps = winningLine == candidateLine || connection p (tail ps)
>                   where 
>                        winningLine = replicate win p
>                        candidateLine = take win ps

PLAYER UTILITIES

> changePlayer :: Player -> Player
> changePlayer O = X
> changePlayer X = O

> isBlank :: Player -> Bool
> isBlank p = p == B

TESTING UTILITIES

> showBoards :: [Board] -> IO ()
> showBoards [] = putStrLn ""
> showBoards (b:bs) =
>                    do
>                      showBoard b
>                      showBoards bs

APPENDIX: PRE-DEFINED BOARDS AND ROWS FOR TESTING, ASSUMING PARAMETER "win" == 4

Cases 1 - 4 for the `allDiagonals` function:

Case 1: X wins diagonally from left top to right bottom below main
"X LEFT DIAGONAL BELOW"

> xldb :: Board
> xldb = [[B,B,B,B,B,B,B],
>         [B,B,B,B,B,B,B],
>         [X,B,B,B,X,B,B],
>         [B,X,B,O,O,B,B],
>         [B,B,X,O,X,B,O],
>         [B,O,O,X,O,X,O]]

Case 2: X wins diagonally from left top to right bottom above main
"X LEFT DIAGONAL ABOVE"

> xlda :: Board
> xlda = [[B,B,B,X,B,B,B],
>         [B,B,B,B,X,B,B],
>         [B,B,B,B,X,X,B],
>         [B,B,B,O,O,B,X],
>         [B,B,X,O,X,B,O],
>         [B,O,O,X,O,X,O]]

Case 3: X wins diagonally from right top to left bottom above main
"X RIGHT DIAGONAL ABOVE"

> xrda :: Board
> xrda = [[B,B,B,X,B,B,B],
>         [B,B,X,B,B,B,B],
>         [O,X,B,B,X,B,X],
>         [X,X,B,O,O,O,B],
>         [B,B,O,O,O,B,O],
>         [B,O,O,X,O,X,O]]

Case 4: X wins diagonally from right top to left bottom below main                       
"X RIGHT DIAGONAL BELOW"

> xrdb :: Board 
> xrdb = [[B,B,B,B,B,B,B],
>         [B,B,B,B,B,B,B],
>         [O,B,B,B,X,B,X],
>         [B,X,B,O,O,X,B],
>         [B,B,O,O,X,B,O],
>         [B,O,O,X,O,X,O]]

Could the AI be made to choose make the second vertical connection (which blocks the human) instead of the first?

> idealPathTest :: Board
> idealPathTest = [[B,B,B,B,B,B,B],
>                  [B,B,B,B,B,B,B],
>                  [B,B,B,B,B,B,B],
>                  [B,B,B,B,X,B,B],
>                  [B,O,B,X,X,O,B],
>                  [B,O,X,X,X,O,B]]  

> fullDraw :: Board
> fullDraw = [[O,X,O,X,O,X,O],
>             [X,O,X,O,X,O,X],
>             [O,O,X,X,O,X,X],
>             [O,X,O,X,X,O,O],
>             [O,X,O,O,X,O,X],
>             [X,O,O,X,X,X,O]]

> arbitraryTest :: Board
> arbitraryTest = [[B,B,B,B,B,B,B],
>                  [B,B,B,B,B,B,B],
>                  [B,B,B,B,B,B,B],
>                  [B,B,B,X,X,B,B],
>                  [B,B,O,O,X,B,B],
>                  [B,O,O,X,X,X,O]]        

> xHorizontalWin :: Board
> xHorizontalWin = [[B,B,B,B,B,B,B],
>                   [B,B,B,B,B,B,B],
>                   [B,B,B,B,B,B,B],
>                   [B,B,B,O,X,B,B],
>                   [B,B,O,O,X,B,B],
>                   [B,O,X,X,X,X,O]]                

> xVerticalWin :: Board
> xVerticalWin = [[B,B,B,B,B,B,B],
>                 [B,B,B,B,B,B,B],
>                 [B,B,B,B,X,B,B],
>                 [B,B,B,O,X,B,B],
>                 [B,B,O,O,X,B,B],
>                 [B,O,O,X,X,X,O]]


X wins in a row

> xr :: Row
> xr = [B,X,X,X,X,O,O]

O wins in a row

> or :: Row
> or = [B,O,O,O,O,X,X]
