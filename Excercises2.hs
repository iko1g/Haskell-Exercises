module Exercises2 (neighbours,findBonding,insertFromCurrentNode,VTree(..),Direction(..),Trail(..),Zipper(..)) where

-- The following two  imports are needed for testing, do not delete
import GHC.Generics (Generic,Generic1)
import Control.DeepSeq

import Data.List 
type Point a = (a,a)
type Metric a = (Point a) -> (Point a) -> Double

-- Exercise A4
neighbours :: Int -> Metric a -> Point a -> [Point a] -> [Point a]
neighbours k d p [] = []
neighbours k d p xs = take k (sortOn (distance p) xs)

distance :: Metric Double
distance (x1,y1) (x2,y2) = sqrt ((dx * dx) + (dy * dy))
  where dx = x2 - x1
        dy = y2 - y1

-- Exercise A5
findBonding :: Eq a => (a -> a -> Bool) -> [a] -> Maybe [(a,a)]
findBonding f xs 
    |length xs `mod`2/=0 =Nothing

-- Exercise A6

data VTree a = Leaf | Node (VTree a) a Int (VTree a) deriving (Eq,Show,Generic,Generic1)
data Direction a = L a Int (VTree a) | R a Int (VTree a) deriving (Eq,Show,Generic,Generic1)
type Trail a = [Direction a]
type Zipper a = (VTree a, Trail a)

instance NFData a => NFData (VTree a)
instance NFData a => NFData (Direction a)

insertFromCurrentNode :: Ord a => a -> Zipper a -> Zipper a
insertFromCurrentNode v z = (Leaf,[])
