module LibTree where

import qualified Data.Tree as T
import Safe (atMay)
import Data.Maybe (isNothing, fromJust)

subForestAtMay :: T.Tree a -> Int -> Maybe ( T.Tree a )
subForestAtMay t i = atMay (T.subForest t) i

treeAtMay :: T.Tree a -> [Int] -> Maybe ( T.Tree a )
treeAtMay t i = foldl (\x y -> if isNothing x then Nothing else subForestAtMay (fromJust x) y) (Just t) i

subForestAt :: T.Tree a -> Int -> T.Tree a
subForestAt t i = (T.subForest t) !! i

treeAt :: T.Tree a -> [Int] -> T.Tree a
treeAt t i = foldl (\x y -> subForestAt x y) t i