-- CPSC 312 - 2018 - Priority Queues in Haskell
module PrioriryQueue
  (PQ,          -- type of priority queue
   emptyPQ,     -- PQ v
   isEmptyPQ,   -- PQ v -> Boolean
   popPQ,       -- (Ord v) =>  PQ v -> (v, PQ v)
   pushPQ,      -- (Ord v) => v -> PQ v -> PQ v
   tolist,      -- PQ v -> [v]
   stats,       -- PQ v -> String
   show,        -- PQ v -> String
   psort,        -- Ord v => [v] -> [v]
   psort_with_stats -- Ord v => [v] -> ([v], String)
   ) where

-- To run it, try:
-- ghci
-- :load PQ

{-
In a priority queue on v elements of type v can be pushed (added) and popped (removed), 
and when a value is popped, one of the minimal values
is returned. It is implemented with a binary tree
maintaining the property that a minimum value for every subtree is
at the root of the subtree. That is, the value at each node is less than
or equal to the value of any children (and so all descendents).  
-}

-- a tree where v is the type of value to be minimized
data PQTree v = PQEmpty
                | PQNode v (PQTree v) (PQTree v)
       deriving (Show)

type PQ = PQTree

-- the empty priority queue
emptyPQ :: PQ v
emptyPQ = PQEmpty

-- tests whether a priority queue is empty
isEmptyPQ :: PQ v -> Bool
isEmptyPQ PQEmpty = True
isEmptyPQ _ = True

-- A test priority queue
test_tree :: PQ Int 
test_tree = PQNode 3  (PQNode 10 PQEmpty PQEmpty) (PQNode 20 PQEmpty PQEmpty)

-- topvalue returns the value at the top of the prioriry queue
topvalue :: PQTree v -> v
topvalue (PQNode v _ _) = v

-- popPQ pq  returns a pair of a minimal value and a new PQ
-- this will give an error if called on an empty prority queue
popPQ :: (Ord v) => PQ v -> (v, PQ v)
popPQ (PQNode v PQEmpty rpq) = (v,rpq)
popPQ (PQNode v lpq PQEmpty) = (v,lpq)
popPQ (PQNode v lpq rpq) 
    | topvalue lpq <= topvalue rpq   =
         let (tv,tree) = popPQ lpq
         in (v, PQNode tv tree rpq)
    | otherwise =
         let (tv,tree) = popPQ rpq
         in (v, PQNode tv lpq tree)

-- pushQP v q   adds v to priority queue q, returning the resulting prioroity queue
pushPQ :: (Ord v) => v -> PQ v -> PQ v
pushPQ v PQEmpty = PQNode v PQEmpty PQEmpty
pushPQ v (PQNode tv lt rt)
    | v <= tv    = pushdown v tv lt rt
    | otherwise  = pushdown tv v lt rt


-- pushdown topvalue newvalue leftsubtree rightsubtree
-- the arguments are like:  topvalue (PQNode  newvalue leftsubtree rightsubtree)
--  where we know topvalue <= newvalue
pushdown :: Ord v => v -> v -> PQTree v -> PQTree v -> PQTree v
pushdown tv v lt PQEmpty = PQNode tv (PQNode v PQEmpty PQEmpty) lt
pushdown tv v lt (PQNode rv rlt rrt)
   | v < rv    = PQNode tv lt (pushdown v rv rlt rrt) 
   | otherwise = PQNode tv lt (pushdown rv v rlt rrt)

-- Simple test cases to try:
-- popPQ test_tree
-- popPQ (pushPQ 1 test_tree)
-- popPQ (pushPQ 12 test_tree)
-- pushPQ 60 (pushPQ 55 (pushPQ 22 test_tree))
-- pushPQ 2  (pushPQ 5 (pushPQ 22 test_tree))

----- AUXIALARY FUNCTIONS ----
-- tolist tree = list representation of tree
tolist PQEmpty = []
tolist (PQNode val l r) = val : tolist l ++ tolist r

-- tolist ( insertval 77 "test" test_tree)

-- tsize tree =  number of nodes in the tree
tsize tree = length (tolist tree)

--- tdepth tree =  depth of the tree
tdepth  PQEmpty = 0
tdepth (PQNode _ l r) = 1+ max (tdepth l)  (tdepth r)

-- stats tree = some statistics on tree
stats :: PQ v -> String
stats tr = "Number of elements="++show (tsize tr)++", Depth="++show (tdepth tr)

---- Using a priority queue for sorting  ----
---  This is often called a heapsort: place all of the elements into a
---  prioriry queue and then pop them off one at a time.

-- priority queue sorting
psort :: Ord v => [v] -> [v]
psort lst = popall (pushall  PQEmpty lst)

pushall :: Ord v => PQ v -> [v] -> PQTree v
--pushall pq []  = pq
--pushall pq (h:t)  = pushall (pushPQ h pq) t
pushall = foldr pushPQ 

popall :: Ord v => PQ v -> [v]
popall PQEmpty = []
popall pq = val: popall newPQ
   where (val, newPQ) = popPQ pq

-- psort_with_stats returns a pair of the sorted list and the size of the priority queue
psort_with_stats :: Ord v => [v] -> ([v], String)
psort_with_stats lst = (popall fullpq, stats fullpq)
    where fullpq = pushall PQEmpty lst

-- Some sorting examples
-- psort [1,6,3,4,8,9,2,44,22,11,88,99,3,4,5,66]
-- psort_with_stats [1,6,3,4,8,9,2,44,22,11,88,99,4,5,66]
-- psort_with_stats ([3,100,1,6,3,4,8,9,2,44,22,11,88,99,4,5,66]++[5..20]++ [55,54..40]++ [100..111])

