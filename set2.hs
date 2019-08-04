{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set2 where

import MCPrelude

--The Maybe Type
data Maybe a = Nothing | Just a

instance Show a => Show (Maybe a) where
    show Nothing = "Nothing"
    show (Just a) = "Just " ++ show a

instance Eq a => Eq (Maybe a) where
    (==) Nothing Nothing = True
    (==) (Just a) (Just b) = a == b
    (==) _ _ = False

--Build a library of things that can fail
headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (a:ax) = Just a

tailMay :: [a] -> Maybe [a]
tailMay [] = Nothing
tailMay (a:ax) = Just ax

lookupMay :: Eq a => a -> [(a,b)] -> Maybe b
lookupMay _ [] = Nothing
lookupMay key (x:xs)    | a == key = Just b
                        | otherwise = lookupMay key xs
    where (a,b) = x

divMay :: (Eq a , Fractional a) => a -> a -> Maybe a
divMay _ 0 = Nothing
divMay a b = Just (a / b) 

maximumMay :: Ord a => [a] -> Maybe a
maximumMay [] = Nothing
maximumMay [a] = Just a
maximumMay (a:b:cx) | a > b = maximumMay (a:cx)
                    | otherwise = maximumMay (b:cx)

minimumMay :: Ord a => [a] -> Maybe a
minimumMay [] = Nothing
minimumMay [a] = Just a
minimumMay (a:b:cx) | a < b = minimumMay (a:cx)
                    | otherwise = minimumMay (b:cx)
