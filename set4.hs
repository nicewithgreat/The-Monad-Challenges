{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set4 where

import MCPrelude

import Set2
import Set3 (Card(..))
{--
--set1
randbyfunction :: (Integer -> a) -> Integer -> a
type Gen a = Seed -> (a , Seed)
generalA :: (a -> b) -> Gen a -> Gen b
generalB :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
getTwo :: Gen a -> (a -> Gen b) -> Gen b
mkGen :: a -> Gen a

--set2
data Maybe a = Nothing | Just a
chain :: (a -> Maybe b) -> Maybe a -> Maybe b
link :: Maybe a -> (a -> Maybe b) -> Maybe b
mkMaybe :: a -> Maybe a
yLink :: Maybe a -> Maybe b -> (a -> b ->  c) -> Maybe c
combine :: Maybe (Maybe a) -> Maybe a

mkMaybe :: a -> Maybe a
combine :: Maybe (Maybe a) -> Maybe a
--}

-- Generalizing State and Maybe
{- 
link :: m a -> (a -> m b) -> m b
yLink :: (a -> b -> c) -> m a -> m b -> m c
 -}

--A Missed Generalization
{- 
-- mkGen :: a -> Gen a
-- getTwo :: Gen a -> (a -> Gen b) -> Gen b
-- generalA :: (a -> b) -> Gen a -> Gen b
generalB2 :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
generalB2 f ga gb = getTwo ga (\a-> getTwo gb (mkGen.f a))

repRandom2 :: [Gen a] -> Gen [a]
repRandom2 (a:ga) = getTwo a ( \x -> generalA (\l -> (:) x l) (repRandom2 ga) )
repRandom2 _ = mkGen []
 -}
 
--Formalizing the Pattern
class Monad m where
    return :: a -> m a
    bind :: m a -> (a -> m b) -> m b

y :: Monad m => (a -> b -> c) -> m a -> m b -> m c
y f ma mb = bind ma (\a -> bind mb (return.f a))

--Creating Instances
instance Monad Maybe where
    return = Just
    bind ma f = case ma of 
        Just a -> f a
        Nothing -> Nothing

instance Monad [] where
    return = (: [])
    bind (a:ax) f = f a ++ bind ax f
    bind _ _ = []

newtype Gen a = Gen { unGen :: Seed -> (a , Seed) }

evalGen :: Gen a -> Seed -> a
evalGen ga s = fst (unGen ga s)

instance Monad Gen where
    --bind ga f = Gen (uncurry (unGen . f) . unGen ga)
    bind ma f = Gen { unGen=(\s -> let (a, s') = unGen ma s in unGen (f a) s') }
    --bind ga f = Gen { unGen=(\s' -> unGen ((f.fst.(unGen ga)) $ s') s') }
    return a = Gen (\s -> (a,s))

--Revisiting Other Generic Functions
liftM2 :: (Monad m) => (a -> b -> c) -> m a -> m b -> m c
liftM2 = y

----set1
sequence :: [Gen a] -> Gen [a]
sequence (a:ga) = bind a (\y -> bind (sequence ga) (\x ->return (y:x)) )
sequence _ = return []

----set2
(=<<) :: (a -> Maybe b) -> Maybe a -> Maybe b
(=<<) = flip bind
join :: Maybe (Maybe a) -> Maybe a
join a = bind a id

----set3
liftM3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
liftM3 f ax bx cx =  bind ax (\a -> bind bx (\b -> bind cx (return.f a b)))
ap :: [a -> b] -> [a] -> [b]
ap [] _ = []
ap _ [] = []
ap fx ax = bind fx (\f -> bind ax (return.f $))

--Using the abstraction
----set1
generalA :: (a -> b) -> Gen a -> Gen b
generalA f ga = bind ga (return.f)

randEven = generalA (*2) (Gen rand)
randOdd = generalA (+1) randEven
randTen = generalA (*10) (Gen rand)
randLetter = generalA toLetter (Gen rand)

randPair :: Gen (Char , Integer)
randPair = liftM2 (,) randLetter (Gen rand)

generalPair :: Gen a -> Gen b -> Gen(a,b)
generalPair = liftM2 (,)

generalB :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
generalB = liftM2

repRandom :: [Gen a] -> Gen [a]
repRandom = sequence

getTwo :: Gen a -> (a -> Gen b) -> Gen b
getTwo = bind

mkGen :: a -> Gen a
mkGen = return
{- 
sequence :: [Gen a] -> Gen [a]
return :: a -> m a
bind :: m a -> (a -> m b) -> m b
liftM2 :: (Monad m) => (a -> b -> c) -> m a -> m b -> m c
y :: Monad m => (a -> b -> c) -> m a -> m b -> m c
evalGen :: Gen a -> Seed -> a
-}

----set2
queryGreek' :: GreekData -> String -> Maybe Double
queryGreek' gd key = join (y (\x y -> divMay (fromIntegral x) (fromIntegral y)) m h)
    where   xs = lookupMay key gd
            m = maximumMay =<< (tailMay =<< xs)
            h = headMay =<< xs

chain' :: (a -> Maybe b) -> Maybe a -> Maybe b
chain' = (=<<)

link' :: Maybe a -> (a -> Maybe b) -> Maybe b
link' = flip chain'

mkMaybe' :: a -> Maybe a
mkMaybe' = return

yLink' :: Maybe a -> Maybe b -> (a -> b ->  c) -> Maybe c
yLink' ma mb f = y f ma mb

addSalaries' :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries' xs n1 n2 = y (+) s1 s2
    where   s1 = lookupMay n1 xs
            s2 = lookupMay n2 xs

transMaybe' :: (a -> b) -> Maybe a -> Maybe b
transMaybe' f = (=<<) (return.f)

tailProd' :: Num a => [a] -> Maybe a
tailProd' = transMaybe' product . tailMay

tailSum' :: Num a => [a] -> Maybe a
tailSum' = transMaybe' sum . tailMay

tailMax' :: Ord a => [a] -> Maybe (Maybe a)
tailMax' = transMaybe' maximumMay . tailMay

tailMin' :: Ord a => [a] -> Maybe (Maybe a)
tailMin' = transMaybe' minimumMay . tailMay

combine :: Maybe (Maybe a) -> Maybe a
combine = join
{- 
(=<<) :: (a -> Maybe b) -> Maybe a -> Maybe b
join :: Maybe (Maybe a) -> Maybe a
return :: a -> m a
bind :: m a -> (a -> m b) -> m b
y :: Monad m => (a -> b -> c) -> m a -> m b -> m c
-}

----set3
allPairs :: [a] -> [b] -> [(a,b)]
allPairs = y (,)

allCards :: [Int] -> [String] -> [Card]
allCards = y Card

allCombs :: (a -> b -> c) -> [a] -> [b] -> [c]
allCombs = y

allCombs3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allCombs3 = liftM3

combStep :: [a -> b] -> [a] -> [b]
combStep = ap

{- 
liftM3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
ap :: [a -> b] -> [a] -> [b]
return :: a -> m a
bind :: m a -> (a -> m b) -> m b
y :: Monad m => (a -> b -> c) -> m a -> m b -> m c
-}