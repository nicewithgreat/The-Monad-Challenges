{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set3 where

import MCPrelude

--Generating combinations
allPairs :: [a] -> [b] -> [(a,b)]
allPairs (a:ax) bx = foldr (\b-> (:) (a,b) ) (allPairs ax bx) bx
allPairs _ _ = []

--Poker hands
data Card = Card Int String
instance Show Card where
    show (Card i s) = show i ++ s

allCards :: [Int] -> [String] -> [Card]
allCards (a:ax) bx = foldr (\b-> (:) (Card a b)) (allCards ax bx) bx
allCards _ _ = []

--Generalizing pairs and cards
allCombs :: (a -> b -> c) -> [a] -> [b] -> [c]
allCombs f (a:ax) bx = foldr (\b-> (:) (f a b)) (allCombs f ax bx) bx
allCombs _ _ _ = []

allPairs' = allCombs (,)
allCards' = allCombs Card

--Combinations of three things
allCombs3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allCombs3 f ax bx cx = allCombs ($) (allCombs f ax bx) cx

--Combinations of more things
combStep :: [a -> b] -> [a] -> [b]
combStep (f:fx) ax = foldr (\a-> (:) (f a)) (combStep fx ax) ax
combStep _ _ = []

allCombs' :: (a -> b -> c) -> [a] -> [b] -> [c]
allCombs' f ax bx = combStep (combStep [f] ax) bx

allCombs3' :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allCombs3' f ax bx cx = combStep (combStep (combStep [f] ax) bx) cx