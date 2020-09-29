{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Search where
import Data.Set as S
import Data.List as L
import Data.Maybe as M



import ProblemState
{-
    *** TODO ***

    Tipul unei nod utilizat în procesul de căutare. Recomandăm reținerea unor
    informații legate de:

    * stare;
    * acțiunea care a condus la această stare;
    * nodul părinte, prin explorarea căruia a fost obținut nodul curent;
    * adâncime
    * copiii, ce vor desemna stările învecinate
-}

data Node s a = Node {state :: s, action :: (Maybe a), parent :: (Maybe (Node s a)), depth :: Int, children :: [Node s a]}
        deriving (Show, Eq, Ord)

{-
    *** TODO ***
    Gettere folosite pentru accesul la câmpurile nodului
-}
nodeState :: Node s a -> s
nodeState node = state node


nodeParent :: Node s a -> Maybe (Node s a)
nodeParent node = parent node

nodeDepth :: Node s a -> Int
nodeDepth node = depth node

nodeAction :: Node s a -> Maybe a
nodeAction node = action node

nodeChildren :: Node s a -> [Node s a]
nodeChildren node = children node

{-
    *** TODO ***

    Generarea întregului spațiu al stărilor
    Primește starea inițială și creează nodul corespunzător acestei stări,
    având drept copii nodurile succesorilor stării curente.
-}
completeNode :: (ProblemState s a, Ord s) => s -> Maybe a -> Int -> Maybe (Node s a) -> Set s -> Node s a
completeNode initialState nodeA nodeD nodeP builtStates = this

            where
                this =  Node initialState nodeA nodeP nodeD
                                         (L.map (\nextState -> (completeNode (snd nextState) (Just (fst nextState)) (nodeD + 1) (Just this)
                                           builtStates)) nChildren) 
                                         
                nChildren = L.filter (((flip S.notMember) builtStates) . snd) $ successors initialState

createStateSpace :: (ProblemState s a, Ord s) => s -> Node s a
createStateSpace initialState =  completeNode initialState Nothing 0 Nothing (S.fromList [initialState])

{-
    *** TODO ***
   
    Primește un nod inițial și întoarce un flux de perechi formate din:
    * lista nodurilor adăugate în frontieră la pasul curent
    * frontiera

-}

bfs :: (ProblemState s a, Ord s) => Node s a -> [([Node s a], [Node s a])]
bfs initialNode = explore S.empty [([initialNode], [initialNode])]
        where
            explore visited result = (recentlyAdded, frontier) : explore newVisited [(nextStates, newFrontier)]
                        where
                        recentlyAdded = fst $ head $ result
                        frontier = snd $ head $ result
                        currentNode = head frontier
                        undexplored = tail frontier
                        newVisited = S.insert (nodeState currentNode) visited
                        nextStates = Prelude.filter (\x -> notMember (nodeState x) visited) $ nodeChildren currentNode
                        newFrontier = undexplored ++ nextStates



{-
    *** TODO ***
  
    Primește starea inițială și finală și întoarce o pereche de noduri, reprezentând
    intersecția dintre cele două frontiere.
-}

bidirBFS :: ( (ProblemState s a, Eq a, Ord s)) => Node s a -> Node s a -> (Node s a, Node s a)
bidirBFS start end = findInt fromStart fromEnd
    
    where
        undefinedNode = (head (fst (head fromStart)), head (fst (head fromEnd)))
        fromStart = bfs start
        fromEnd = bfs end 
        findInt [] _ = undefinedNode
        findInt _ [] = undefinedNode
        findInt (top_a : rest_a) (top_b : rest_b) 
            |pair == Nothing = findInt rest_a rest_b
            |otherwise = M.fromJust pair

            where
                pair = findPair newAdded_a frontier_b
                newAdded_a = fst top_a
                frontier_b = snd top_b
                findPair [] _ = Nothing
                findPair (el:rest) list
                            |listFiltered == [] = findPair rest list
                            |otherwise = (Just (el, head listFiltered))
                        where
                            listFiltered = L.filter (\current -> (nodeState current == nodeState el)) list
{-
    *** TODO ***

    Pornind de la un nod, reface calea către nodul inițial, urmând legăturile
    către părinți.

    Întoarce o listă de perechi (acțiune, stare), care pornește de la starea inițială
    și se încheie în starea finală.

-}

extractPath :: (Eq a, Eq s) => Node s a -> [(Maybe a, s)]
extractPath node = goUp (Just node) []
        where
            goUp currentNode result
                |currentNode == Nothing = result 
                |otherwise = goUp (nodeParent justNode) (((nodeAction justNode), (nodeState justNode)) : result)
                
                where
                    justNode = M.fromJust currentNode

{-
    *** TODO ***

    Pornind de la o stare inițială și una finală, se folosește de bidirBFS pentru a găsi
    intersecția dintre cele două frontiere și de extractPath pentru a genera calea.

    Atenție: Pentru calea gasită în a doua parcurgere, trebuie să aveți grijă la a asocia
    corect fiecare stare cu acțiunea care a generat-o.

    Întoarce o listă de perechi (acțiune, stare), care pornește de la starea inițială
    și se încheie în starea finală.
-}

solve :: (ProblemState s a, Eq a, Ord s)
      => s          -- Starea inițială de la care se pornește
      -> s          -- Starea finală la care se ajunge
      -> [(Maybe a, s)]   -- Lista perechilor
solve start final = path

    where
        intersec = bidirBFS (createStateSpace start) (createStateSpace final)
        firstPath = extractPath $ fst intersec
        secondPath = L.map (\stateS -> ((fromJust (fst stateS)), snd stateS)) (tail $ extractPath $ snd intersec)
        secondPathRevA = L.map (\stateS -> 
                                let revState = reverseAction stateS
                                in ((Just (fst revState)), (snd revState))) secondPath
        path = firstPath ++ (reverse secondPathRevA)
            