{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}

module RollTheBall where
import Pipes
import ProblemState

import Data.Array as A
import Data.List (elemIndex)

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
data Cell = Cell { c_type :: Char
}
    deriving (Show, Eq, Ord)

{-
    Tip de date pentru reprezentarea nivelului curent
-}
data Level = Level (A.Array Position Cell) Position Position
    deriving (Ord)
instance Eq Level where
    Level array1 _ _ == Level array2 _ _ = assocs array1 == assocs array2

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

elementAt :: (A.Array Position Cell) -> Position -> Cell
elementAt arr pos = arr A.! pos

lowerRightCorner :: (A.Array Position Cell) -> Position
lowerRightCorner = snd . A.bounds

lowerLeftCorner :: (A.Array Position Cell) -> Position
lowerLeftCorner = fst . A.bounds



instance Show Level
    where show (Level lvl _ _) = concat [endl : (map c_type [elementAt lvl (i,j) | j <- [0..m]])| i <- [0..n]] ++ [endl]

            where
                n = fst $ lowerRightCorner lvl
                m = snd $ lowerRightCorner lvl


{-
    *** TODO ***
    Primește coordonatele colțului din dreapta jos a hărții.
    Intoarce un obiect de tip Level în care tabla este populată
    cu EmptySpace. Implicit, colțul din stânga sus este (0,0)
-}

emptyLevel :: Position -> Level
emptyLevel pos = Level  (A.array ((0,0), pos) [((i, j), Cell emptySpace) | i <- [0..fst pos], j <- [0..snd pos]]) (-1, -1) (-1, -1)


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
movingCell :: Position -> Level -> Bool
movingCell pos (Level lvl _ _)
    |cell_type == verPipe || cell_type == topLeft || cell_type == botLeft || cell_type == botRight
                          || cell_type == topRight || cell_type == horPipe || cell_type == emptyCell = True
    |otherwise = False

    
    where
        cell_type = c_type (elementAt lvl pos)
validPos :: Position -> Int -> Int -> Bool
validPos pos n m = not (x < 0 || y < 0 || x > n || y > m)
                where
                    x = fst pos
                    y = snd pos
addCell :: (Char, Position) -> Level -> Level
addCell (value, pos) (Level lvl start win)
    |(validPos pos n m) && (c_type (elementAt lvl pos) == emptySpace) = Level (lvl A.// [(pos, Cell value)]) newStart newWin
    |otherwise = Level lvl start win

    where 
        n = fst $ lowerRightCorner lvl
        m = snd $ lowerRightCorner lvl
        newStart = case value `elemIndex` startCells of
                  Just _ -> pos
                  Nothing -> start
        newWin = case value `elemIndex` winningCells of
                Just _ -> pos
                Nothing -> win
    

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
createLevel pos list = foldr (\level element -> addCell level element) (emptyLevel pos) list 


{-
    *** TODO ***

    Mișcarea unei celule în una din cele 4 direcții 
    Schimbul se poate face doar dacă celula vecină e goală (emptySpace).
    Celulele de tip start și win sunt imutabile.

    Hint: Dacă nu se poate face mutarea puteți lăsa nivelul neschimbat.
-}
isStart :: Cell -> Bool
isStart (Cell c) = (c == startDown || c == startUp || c == startLeft || c == startRight)

isWin :: Cell -> Bool
isWin (Cell c)  = (c == winDown || c == winUp || c == winLeft || c == winRight)

isOutside :: Position -> Directions -> Int -> Int -> Bool
isOutside pos direc n m

            | x == 0 = direc == North
            | y == 0 = direc == West
            | x == n = direc == South
            | y == m = direc == East
            | otherwise = not $ validPos pos n m
            where
                x = fst pos
                y = snd pos

isOccupied :: Level -> Position -> Directions -> Bool
isOccupied (Level lvl _ _) pos dir
    
            |dir == North = c_type (elementAt lvl (x - 1, y)) /= emptyCell
            |dir == South = c_type (elementAt lvl (x + 1, y)) /= emptyCell
            |dir == East = c_type (elementAt lvl (x, y + 1)) /= emptyCell
            |dir == West = c_type (elementAt lvl (x, y - 1)) /= emptyCell
            |otherwise = False
            where 
                x = fst pos
                y = snd pos
            
swapCells :: Level -> Cell -> Position -> Directions -> Level
swapCells (Level lvl start win) cell pos dir
            |dir == North = Level (lvl A.// [(pos, Cell emptySpace), ((x - 1, y), cell)]) start win
            |dir == South = Level (lvl A.// [(pos, Cell emptySpace), ((x + 1, y), cell)]) start win
            |dir == East = Level (lvl A.// [(pos, Cell emptySpace), ((x, y + 1), cell)]) start win
            |dir == West = Level (lvl A.// [(pos, Cell emptySpace), ((x, y - 1), cell)]) start win
            |otherwise = Level lvl start win
            where
                x = fst pos
                y = snd pos

moveCell :: Position -> Directions -> Level -> Level
moveCell pos dir (Level lvl start win)
        | (isWin cell || isStart cell) || (isOutside pos dir n m) || (not (isOccupied (Level lvl start win) pos dir)) = Level lvl start win
        |otherwise = swapCells (Level lvl start win) cell pos dir

        where
            cell = elementAt lvl pos
            n = fst $ lowerRightCorner lvl
            m = snd $ lowerRightCorner lvl
            

{-
    *** TODO ***

    Verifică dacă două celule se pot conecta.
    Atenție: Această funcție se aplică de la stânga la 
    dreapta(nu este comutativă).

    ex: connection botLeft horPipe = True (╚═)
        connection horPipe botLeft = False (═╚)
-}
connection :: Cell -> Cell -> Directions -> Bool
connection (Cell type_a) (Cell type_b) dir
            |type_a == startUp =
                    case () of
                        () |dir == North -> (type_b == topRight || type_b == topLeft || type_b == verPipe || type_b == winDown)
                           |otherwise -> False
            |type_a == startDown =
                    case () of
                        () |dir == South -> (type_b == botRight || type_b == botLeft || type_b == verPipe || type_b == winUp)
                           |otherwise -> False
            |type_a == startLeft = 
                    case () of
                        () |dir == West -> (type_b == topLeft || type_b == botLeft || type_b == horPipe || type_b == winRight)
                           |otherwise -> False     
            |type_a == startRight =
                    case () of
                        () |dir == East -> (type_b == botRight || type_b == topRight || type_b == horPipe || type_b == winLeft)
                           |otherwise -> False              
            |type_a == botLeft = 
                    case () of
                        () |dir == East -> (type_b == horPipe || type_b == botRight || type_b == startLeft || type_b == winLeft)
                           |dir == North -> (type_b == verPipe || type_b == topLeft || type_b == topRight || type_b == startDown || type_b == winDown)
                           |otherwise -> False
            |type_a == horPipe =
                    case () of
                        () |dir == East -> (type_b == botRight ||type_b == topRight || type_b == startLeft || type_b == winLeft)
                           |dir == West -> (type_b == topLeft || type_b == botLeft || type_b == startRight || type_b == winRight)
                           |otherwise -> False
            |type_a == verPipe =
                    case () of
                        () |dir == South -> (type_b == botLeft || type_b == botRight || type_b == startUp || type_b == winUp)
                           |dir == North -> (type_b == topLeft || type_b == topRight || type_b == startDown || type_b == winDown)
                           |otherwise -> False
            |type_a == topLeft =
                    case () of
                        () |dir == East -> (type_b == horPipe || type_b == topRight || type_b == startLeft || type_b == winLeft)
                           |dir == South -> (type_b == botLeft || type_b == verPipe || type_b == startUp || type_b == winUp)
                           |otherwise -> False
            |type_a == topRight =
                    case () of
                        () |dir == South -> (type_b == verPipe || type_b == botRight || type_b == startUp || type_b == winUp)
                           |dir == West -> (type_b == horPipe || type_b == topLeft || type_b == startRight || type_b == winRight)
                           |otherwise -> False
            |otherwise = False
            
{-
    *** TODO ***

    Va returna True dacă jocul este câștigat, False dacă nu.
    Va verifica dacă celulele cu Pipe formează o cale continuă de la celula
    de tip inițial la cea de tip final.
    Este folosită în cadrul Interactive.
-}
getStart :: Cell -> Directions -> Cell
getStart (Cell c) dir
    |c == horPipe = 
            case () of
                () |dir == West -> Cell startRight
                   |dir == East -> Cell startLeft
                   |otherwise -> Cell emptyCell
    |c == verPipe =
            case () of
                () |dir == South -> Cell startUp
                   |dir == North -> Cell startDown
                   |otherwise -> Cell emptyCell

    |c == topLeft =
            case () of
                () |dir == East -> Cell startDown
                   |dir == South -> Cell startRight
                   |otherwise -> Cell emptyCell
    |c == botLeft =
            case () of
                () |dir == East -> Cell startUp
                   |dir == North -> Cell startRight
                   |otherwise -> Cell emptyCell
    |c == botRight =
            case () of
                () |dir == West -> Cell startUp
                   |dir == North -> Cell startLeft
                   |otherwise -> Cell emptyCell
    |c == topRight =
            case () of
                () |dir == West -> Cell startDown
                   |dir == South -> Cell startLeft
                   |otherwise -> Cell emptyCell

    |otherwise = Cell emptyCell

wonLevel :: Level -> Bool
wonLevel (Level lvl start win)
    |start == win = True
    |c_type start_cell == startUp =
                if (validPos (fst start - 1, snd start) n m)
                    then if (connection start_cell (elementAt lvl (fst start - 1, snd start)) North)
                        then wonLevel $ Level (lvl A.// [((fst start - 1, snd start), 
                                              (getStart (elementAt lvl (fst start - 1, snd start)) South))]) (fst start - 1, snd start) win
                        else False
                else False
    |c_type start_cell == startDown =
                if (validPos (fst start + 1, snd start) n m)
                    then if (connection start_cell (elementAt lvl (fst start + 1, snd start)) South)
                        then wonLevel $ Level (lvl A.// [((fst start + 1, snd start),
                                              (getStart (elementAt lvl (fst start + 1, snd start)) North))]) (fst start + 1, snd start) win
                        else False
                else False
    |c_type start_cell == startRight =
            if (validPos (fst start, snd start + 1) n m)
                then if (connection start_cell (elementAt lvl (fst start, snd start + 1)) East)
                    then wonLevel $ Level (lvl A.// [((fst start, snd start + 1),
                                            (getStart (elementAt lvl (fst start , snd start + 1)) West))]) (fst start, snd start + 1) win
                    else False
            else False
    |c_type start_cell == startLeft =
        if (validPos (fst start, snd start - 1) n m)
            then if (connection start_cell (elementAt lvl (fst start, snd start - 1)) West)
                then wonLevel $ Level (lvl A.// [((fst start, snd start - 1),
                                        (getStart (elementAt lvl (fst start, snd start - 1)) East))]) (fst start, snd start - 1) win
                else False
        else False
    |otherwise = True
    where
        start_cell = elementAt lvl start
        n = fst $ lowerRightCorner lvl
        m = snd $ lowerRightCorner lvl


getEmptySpaces:: Level -> [Position]
getEmptySpaces (Level lvl _ _) = filter (\pos -> c_type (elementAt lvl pos) == emptySpace) [(i,j) | i <- [0..n], j <- [0..m]]
            where
                n = fst $ lowerRightCorner lvl
                m = snd $ lowerRightCorner lvl
extendSpace :: Position -> [(Position, Directions)]
extendSpace pos =
        map (\x -> ((fst (fst x) + fst pos, snd (fst x) + snd pos), snd x)) extendPos
        where
            extendPos = [((0,1), West), ((0,-1), East), ((1,0), North), ((-1,0), South)]

addState :: Position -> Level -> [((Position, Directions), Level)]
addState pos (Level lvl start win) =
        map (\move -> (move, Level (lvl A.// [(pos, (elementAt lvl (fst move))), (fst move, Cell emptySpace)]) start win)) validStates
        where
            validStates = filter (\x -> (validPos (fst x) n m) && (movingCell (fst x) (Level lvl start win))) extendedPos
            extendedPos = extendSpace pos
            n = fst $ lowerRightCorner lvl
            m = snd $ lowerRightCorner lvl            

instance ProblemState Level (Position, Directions) where
    successors level = concat $ map (\pos -> (addState pos level)) spaces

        where
            spaces = getEmptySpaces level

    isGoal level = wonLevel level
    reverseAction ((pos, dir), level)
        |dir == South = ((((fst pos) + 1, snd pos), North), moveCell ((fst pos) + 1, snd pos) North level)
        |dir == North = ((((fst pos) - 1, snd pos), South), moveCell ((fst pos) - 1, snd pos) South level)
        |dir == East = (((fst pos, (snd pos) + 1), West), moveCell (fst pos, (snd pos) + 1) West level)
        |dir == West = (((fst pos, (snd pos) - 1), East), moveCell (fst pos, (snd pos) - 1) East level)
        |otherwise = ((pos,dir), level)
