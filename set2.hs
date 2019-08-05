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

--Chains of Failing Computations
queryGreek :: GreekData -> String -> Maybe Double
queryGreek gd key = case lookupMay key gd of
    Just xs -> case tailMay xs of
        Just tx -> case maximumMay tx of
            Just m -> case headMay xs of
                Just h -> divMay (fromIntegral m) (fromIntegral h)
                Nothing -> Nothing
            Nothing -> Nothing
        Nothing -> Nothing
    Nothing -> Nothing

--Generalizing chains of failures
chain :: (a -> Maybe b) -> Maybe a -> Maybe b
chain f ma = case ma of 
    Just a -> f a
    Nothing -> Nothing

link :: Maybe a -> (a -> Maybe b) -> Maybe b
link = flip chain

{--
queryGreek2 :: GreekData -> String -> Maybe Double
queryGreek2 gd key = link jh ( (link jm ((divMay.fromIntegral) $) ).fromIntegral $)
    where   xs = lookupMay key gd
            jm = link (link xs tailMay) maximumMay
            jh = link xs headMay
--}
queryGreek2 :: GreekData -> String -> Maybe Double
queryGreek2 d s = chain (\m -> chain (divMay (fromIntegral m) . fromIntegral) mh) mm
    where xs = lookupMay s d
          mm = chain maximumMay . chain tailMay $ xs
          mh = chain headMay xs

--Chaining variations
mkMaybe :: a -> Maybe a
mkMaybe = Just

yLink :: Maybe a -> Maybe b -> (a -> b ->  c) -> Maybe c
yLink ma mb f = link ma (\a -> (link mb (mkMaybe.f a)) )
----chain (\a -> chain (f a) mb) ma
----addSalaries :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries2 :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries2 xs n1 n2 = yLink s1 s2 (+)
    where   s1 = lookupMay n1 xs
            s2 = lookupMay n2 xs

--Tailprod
tailProd :: Num a => [a] -> Maybe a
tailProd = transMaybe product . tailMay
--tailMay :: [a] -> Maybe [a]
--product :: (Num a, Data.Foldable.Foldable t) => t a -> a
--transMaybe :: (a -> b) -> Maybe a -> Maybe b--..wow..

tailSum :: Num a => [a] -> Maybe a
tailSum = transMaybe sum . tailMay

transMaybe :: (a -> b) -> Maybe a -> Maybe b
transMaybe f (Just a) = mkMaybe.f $ a 

tailMax :: Ord a => [a] -> Maybe (Maybe a)
tailMax = transMaybe maximumMay . tailMay
--tailMay :: [a] -> Maybe [a]
--maximumMay :: Ord a => [a] -> Maybe a
tailMin :: Ord a => [a] -> Maybe (Maybe a)
tailMin = transMaybe minimumMay . tailMay

combine :: Maybe (Maybe a) -> Maybe a
combine (Just a) = a
combine _ = Nothing