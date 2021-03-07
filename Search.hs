{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Search where

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

data Node s a = Node s (Maybe (Node s a)) Int (Maybe a) [(Node s a)]

{-
    *** TODO ***
    Gettere folosite pentru accesul la câmpurile nodului
-}

instance (Eq s) => (Eq (Node s a))
    where
        nod1 == nod2 = (nodeState nod1) == (nodeState nod2);
        nod1 /= nod2 = not ((nodeState nod1) == (nodeState nod2));

instance (Ord s) => (Ord (Node s a))
    where
        nod1 > nod2 = (nodeDepth nod1) > (nodeDepth nod2);
        nod1 >= nod2 = (nodeDepth nod1) >= (nodeDepth nod2);
        nod1 < nod2 = (nodeDepth nod1) < (nodeDepth nod2);
        nod1 <= nod2 = (nodeDepth nod1) <= (nodeDepth nod2);


nodeState :: Node s a -> s
nodeState (Node a _ _ _ _) = a

nodeParent :: Node s a -> Maybe (Node s a)
nodeParent (Node _ x _ _ _) = x

nodeDepth :: Node s a -> Int
nodeDepth (Node _ _ depth _ _) = depth

nodeAction :: Node s a -> Maybe a
nodeAction (Node _ _ _ a _) = a

nodeChildren :: Node s a -> [(Node s a)]
nodeChildren (Node _ _ _ _ lst) = lst

{-
    *** TODO ***

    Generarea întregului spațiu al stărilor
    Primește starea inițială și creează nodul corespunzător acestei stări,
    având drept copii nodurile succesorilor stării curente.
-}


listToNode::(ProblemState s a, Eq s) => [(a, s)] -> Maybe (Node s a) -> Int -> [Node s a]
listToNode lst daddy depth = (foldl (\y (mv, s) -> y ++ [(nextNode s mv)]) [] lst)
    where
        nextNode a b = (Node a daddy depth (Just b) (nextList a b));
        nextList a b = (listToNode (successors a) (Just (nextNode a b)) (depth + 1))

createStateSpace :: (ProblemState s a, Eq s) => s -> Node s a
createStateSpace lvl = (Node lvl Nothing 0 Nothing (listToNode (successors lvl) (Just (createStateSpace lvl)) 1))

{-
    *** TODO ***
   
    Primește un nod inițial și întoarce un flux de perechi formate din:
    * lista nodurilor adăugate în frontieră la pasul curent
    * frontiera

-}

bfsToNext::Ord s => Node s a -> [Node s a] -> [([Node s a], [Node s a])]
-- bfsToNext nod (lst1, lst2) = (lst1, lst2) ++ bfsToNext((head lst2) , [((nodeChildren nod), (tail lst2) ++ (nodeChildren nod))]) 
bfsToNext nod lst
    | null (nodeChildren nod) = []
    | otherwise = [((nodeChildren nod), newFront)] ++ (bfsToNext (head newFront) (tail newFront))
    where
        newFront = lst ++ (nodeChildren nod)


bfs :: Ord s => Node s a -> [([Node s a], [Node s a])]
bfs nod = [([nod], [nod])] ++ (bfsToNext nod [])


{-
    *** TODO ***
  
    Primește starea inițială și finală și întoarce o pereche de noduri, reprezentând
    intersecția dintre cele două frontiere.
-}

bidirBFS :: Ord s => Node s a -> Node s a -> (Node s a, Node s a)
bidirBFS nod1 nod2 = undefined


{-
    *** TODO ***

    Pornind de la un nod, reface calea către nodul inițial, urmând legăturile
    către părinți.

    Întoarce o listă de perechi (acțiune, stare), care pornește de la starea inițială
    și se încheie în starea finală.

-}

extractPath :: Node s a -> [(Maybe a, s)]
extractPath = undefined

{-
    *** TODO ***

    Pornind de la o stare inițială și una finală, se folosește de bidirBFS pentru a găsi
    intersecția dintre cele două frontiere și de extractPath pentru a genera calea.

    Atenție: Pentru calea gasită în a doua parcurgere, trebuie să aveți grijă la a asocia
    corect fiecare stare cu acțiunea care a generat-o.

    Întoarce o listă de perechi (acțiune, stare), care pornește de la starea inițială
    și se încheie în starea finală.
-}

solve :: (ProblemState s a, Ord s)
      => s          -- Starea inițială de la care se pornește
      -> s          -- Starea finală la care se ajunge
      -> [(Maybe a, s)]   -- Lista perechilor
solve = undefined
