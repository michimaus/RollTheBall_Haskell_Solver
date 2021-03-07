{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}
module RollTheBall where
import Pipes
import ProblemState
-- import RollLevels

import Data.List as L

import Data.Array as A

{-
    Direcțiile în care se poate mișca o piesa pe tablă
-}

data Directions = North | South | West | East
    deriving (Show, Eq, Ord)

{-
    Sinonim tip de date pentru reprezetarea unei perechi (Int, Int)
    care va reține coordonatele celulelor de pe tabla de joc
-}

type Position = (Int, Int)

{-
    Tip de date pentru reprezentarea celulelor tablei de joc
-}

data KidsOfCells = StartCell | EndCell | BlockCell | EmptyCell deriving (Eq, Ord)

-- data Cell = Cell Position [Directions] deriving (Show, Eq, Ord)
data Cell = Cell Position Char Bool deriving (Eq, Ord)

getCellDir::Cell -> [Directions]
getCellDir (Cell _ idx _)
    | idx == '═' = [West, East]
    | idx == '║' = [North, South]
    | idx == '╔' = [South, East]
    | idx == '╚' = [North, East]
    | idx == '╝' = [North, West]
    | idx == '╗' = [South, West]
    | idx == '░' || idx == '▓' = []
    | idx == '┬' || idx == '╥' = [South]
    | idx == '┴' || idx == '╨' = [North]
    | idx == '┤' || idx == '╡' = [West]
    | otherwise = [East]

canCellMove::Cell -> Bool
canCellMove (Cell _ _ mv) = mv

findCharMatch::Char -> [Char] -> Bool
findCharMatch idx lst
    | null lst = False
    | idx == (head lst) = True
    | otherwise = findCharMatch idx (tail lst)

getCellKind::Char -> KidsOfCells
getCellKind chr
    | (findCharMatch chr startCells) == True = StartCell
    | (findCharMatch chr winningCells) == True = EndCell
    | chr == emptySpace = EmptyCell
    | otherwise = BlockCell

{-
    Tip de date pentru reprezentarea nivelului curent
-}

data Level = Level Position (A.Array (Int, Int) Cell) Position
    deriving (Eq, Ord)
{-
    *** Optional *** 
  
    Dacă aveți nevoie de o funcționalitate particulară,
    instantiați explicit clasele Eq și Ord pentru Level.
    În cazul acesta, eliminați deriving (Eq, Ord) din Level.
-}

{-
    *** TODO ***

    Instanțiati Level pe Show. 
    Atenție! Fiecare linie este urmată de \n (endl in Pipes).
-}


instance Show Level
    where
        show lvl = endl : (cellsContentLst $ addBackSpace lvl)

addBackSpace:: Level -> (A.Array (Int, Int) Cell)
addBackSpace (Level (lins, cols) arr _) = newArray
    where
        newArray = A.array ((0, 0), (lins, cols + 1)) newElements;
        newElements = currentElements ++ backSpaces;
        currentElements = [((getPosCell x), x) | x <- (cellsLst arr)];
        backSpaces = [((i, cols + 1), (Cell (i, cols + 1) endl True)) | i <- [0 .. lins]]

chrOfCell::Cell -> Char
chrOfCell (Cell _ chr _) = chr


chrAtCelPos :: (A.Array (Int, Int) Cell) -> (Int, Int) -> Char
chrAtCelPos arr pos = (chrOfCell (arr A.! pos))

celAtPos :: (A.Array (Int, Int) Cell) -> (Int, Int) -> Cell
celAtPos arr pos = arr A.! pos

cellsContentLst :: (A.Array (Int, Int) Cell) -> [Char]
cellsContentLst arr = map chrOfCell (A.elems arr)

cellsLst :: (A.Array (Int, Int) Cell) -> [Cell]
cellsLst arr = A.elems arr

willCellMove::Char -> Bool
willCellMove chr
    | ((getCellKind chr == StartCell) || (getCellKind chr == EndCell)) = False
    | otherwise = True

getPosCell::Cell -> Position
getPosCell (Cell pos _ _) = pos

{-
    *** TODO ***
    Primește coordonatele colțului din dreapta jos a hărții.
    Intoarce un obiect de tip Level în care tabla este populată
    cu EmptySpace. Implicit, colțul din stânga sus este (0,0)
-}

emptyLevel :: Position -> Level
emptyLevel (lins, cols) = (Level (lins, cols) arr (-1, -1))
    where
        arr = (A.array ((0, 0), (lins, cols)) [((i, j), (Cell (i, j) '░' True)) | i <- [0 .. lins], j <- [0 .. cols]])

{-
    *** TODO ***

    Adaugă o celulă de tip Pipe în nivelul curent.
    Parametrul char descrie tipul de tile adăugat: 
        verPipe -> pipe vertical
        horPipe -> pipe orizontal
        topLeft, botLeft, topRight, botRight -> pipe de tip colt
        startUp, startDown, startLeft, startRight -> pipe de tip initial
        winUp, winDown, winLeft, winRight -> pipe de tip final
    Parametrul Position reprezintă poziția de pe hartă la care va fi adaugată
    celula, dacă aceasta este liberă (emptySpace).
-}

compStartCell::Char -> Position -> Position -> Position
compStartCell chr (i1, j1) (i2, j2)
    | i1 /= -1 && j1 /= -1 = (i1, j1)
    | (L.find (== chr) startCells) == Nothing  = (i1, j1)
    | otherwise = (i2, j2)


isPosValid::Position -> Position -> Bool
isPosValid (ic, jc) (lins, cols)
    | ic < 0 || jc < 0 || ic > lins || jc > cols = False
    | otherwise = True


addCell :: (Char, Position) -> Level -> Level
addCell (chr, pos) (Level dims arr fstCel)
    | isPosValid pos dims == False = (Level dims arr fstCel)
    | chrAtCelPos arr pos /= '░' = (Level dims arr fstCel)
    | otherwise = (Level dims updatedArr (compStartCell chr fstCel pos))
    where
        updatedArr = (arr A.// [(pos, (Cell pos chr (willCellMove chr)))])

{-
    *** TODO *** 

    Primește coordonatele colțului din dreapta jos al hărții și o listă de 
    perechi de tipul (caracter_celulă, poziția_celulei).
    Întoarce un obiect de tip Level cu toate celeule din listă agăugate pe
    hartă.
    Observatie: Lista primită ca parametru trebuie parcursă de la dreapta 
    la stanga.
-}

createLevel :: Position -> [(Char, Position)] -> Level
createLevel dims lst = foldl (\x y -> (addCell y x)) (emptyLevel dims) lst


{-
    *** TODO ***

    Mișcarea unei celule în una din cele 4 direcții 
    Schimbul se poate face doar dacă celula vecină e goală (emptySpace).
    Celulele de tip start și win sunt imutabile.

    Hint: Dacă nu se poate face mutarea puteți lăsa nivelul neschimbat.
-}

getNextPos::Position -> Directions -> Position
getNextPos (ic, lc) dir
    | dir == North = (ic - 1, lc)
    | dir == East = (ic, lc + 1)
    | dir == South = (ic + 1, lc)
    | otherwise = (ic, lc - 1)

areDifCells::Char -> Char -> Bool
areDifCells chr1 chr2
    | chr1 == '░' && chr2 == '░' = False
    | chr1 /= '░' && chr2 /= '░' = False
    | otherwise = True

moveCell :: Position -> Directions -> Level -> Level
moveCell pos dir (Level dims arr fstCel)
    | (isPosValid pos dims) == False || (isPosValid (getNextPos pos dir) dims) == False = (Level dims arr fstCel)
    | (canCellMove (celAtPos arr pos)) == False = (Level dims arr fstCel)
    | (canCellMove (celAtPos arr (getNextPos pos dir))) == False = (Level dims arr fstCel)
    | (areDifCells (chrAtCelPos arr (getNextPos pos dir)) (chrAtCelPos arr pos)) == False = (Level dims arr fstCel)
    | otherwise = (Level dims (arr A.// [(pos, cell1), ((getNextPos pos dir), cell2)]) fstCel)
    where
        cell1  = (Cell pos (chrAtCelPos arr (getNextPos pos dir)) True);
        cell2  = (Cell (getNextPos pos dir) (chrAtCelPos arr pos) True)

{-
    *** HELPER ***

    Verifică dacă două celule se pot conecta.
    Atenție: Direcția indică ce vecin este a
    doua celulă pentru prima.

    ex: connection botLeft horPipe East = True (╚═)
        connection horPipe botLeft East = False (═╚)
-}

oposDir::Directions -> Directions
oposDir dir
    | dir == East = West
    | dir == West = East
    | dir == North = South
    | otherwise = North


connection :: Cell -> Cell -> Directions -> Bool
connection cel1 cel2 dir
    | (L.find (== dir) (getCellDir cel1)) == Nothing = False
    | (L.find (== (oposDir dir)) (getCellDir cel2)) == Nothing = False
    | otherwise = True

{-
    *** TODO ***

    Va returna True dacă jocul este câștigat, False dacă nu.
    Va verifica dacă celulele cu Pipe formează o cale continuă de la celula
    de tip inițial la cea de tip final.
    Este folosită în cadrul Interactive.
-}

-- getNextPos

getPosStart::Level -> Position
getPosStart (Level _ _ pos) = pos


getToArray::(A.Array (Int, Int) Cell) -> Position -> Directions -> Position -> Bool
getToArray arr pos initDir dims
    | null dirLst == True = True
    -- | (L.find (== (chrOfCell (arr A.! pos))) winningCells) /= Nothing = True
    | (isPosValid (getNextPos pos (head dirLst)) dims) == False = False
    | (connection (arr A.! pos) (arr A.! (getNextPos pos (head dirLst))) (head dirLst)) == False = False
    | otherwise = (getToArray arr (getNextPos pos (head dirLst)) (oposDir (head dirLst)) dims)
    where
        dirLst = filter (/= initDir) (getCellDir (arr A.! pos))

wonLevel :: Level -> Bool
wonLevel (Level dims arr stPos)
    | stPos == (-1, -1) = False
    | (isPosValid nextPos dims) == False = False
    | (connection (arr A.! stPos) (arr A.! nextPos) toDir) == False = False
    | otherwise = getToArray arr nextPos nextFromDir dims
    where
        toDir = head $ getCellDir (arr A.! stPos);
        nextFromDir = oposDir toDir;
        nextPos = (getNextPos stPos toDir)

-- stuckCell:: Level -> Position
-- stuckCell (Level dims arr stPos)
--     | (isPosValid nextPos dims) = stuckCell2 arr nextPos nextFromDir dims
--     | otherwise = stPos
--     where
--         toDir = head $ getCellDir (arr A.! stPos);
--         nextFromDir = oposDir toDir;
--         nextPos = (getNextPos stPos toDir)

-- stuckCell2::(A.Array (Int, Int) Cell) -> Position -> Directions -> Position -> Position
-- stuckCell2 arr pos initDir dims
--     | null dirLst == True = pos
--     -- | (L.find (== (chrOfCell (arr A.! pos))) winningCells) /= Nothing = pos
--     -- | (isPosValid (getNextPos pos dir) dims) == False = pos
--     -- | (connection (arr A.! pos) (arr A.! (getNextPos pos dir)) dir) == False = pos
--     -- | otherwise = (stuckCell2 arr (getNextPos pos dir) (oposDir dir) dims)
--     | otherwise = (stuckCell2 arr (getNextPos pos (head dirLst)) (oposDir (head dirLst)) dims)
--     where
--         dirLst = filter (/= initDir) (getCellDir (arr A.! pos))


canMoveThis :: Position -> Directions -> Level -> Bool
canMoveThis pos dir (Level dims arr _)
    | (isPosValid pos dims) == False || (isPosValid (getNextPos pos dir) dims) == False = False
    | (chrOfCell (arr A.! (getNextPos pos dir))) /= '░' = False
    | (canCellMove (celAtPos arr pos)) == False = False
    | (canCellMove (celAtPos arr (getNextPos pos dir))) == False = False
    | (areDifCells (chrAtCelPos arr (getNextPos pos dir)) (chrAtCelPos arr pos)) == False = False
    | otherwise = True

dirHere :: [Directions]
dirHere = [East, West, South, North];

getArrLvl:: Level -> (A.Array (Int, Int) Cell)
getArrLvl (Level _ arr _) = arr

getNextLevels::Level -> Cell -> [((Position, Directions), Level)]
getNextLevels lvl (Cell pos _ _) = theLst
    where
        theLst = (foldl (\y x -> if (canMoveThis pos x lvl) == False then y ++ [] else y ++ [((pos, x), (moveCell pos x lvl))]) [] dirHere);


instance ProblemState Level (Position, Directions) where
    successors lvl = foldl (\y x -> y ++ (getNextLevels lvl x)) [] (A.elems $ getArrLvl lvl)
    isGoal = wonLevel
    reverseAction ((pos, dir), lvl) = ((prevPos, otherDir), (moveCell pos otherDir lvl))
        where
            otherDir = oposDir dir;
            prevPos = getNextPos pos otherDir
