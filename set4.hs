{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set4 where

import MCPrelude
import Set2

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
    bind = link

{- 
    chain :: (a -> Maybe b) -> Maybe a -> Maybe b
    chain f ma = case ma of 
        Just a -> f a
        Nothing -> Nothing
    
    link :: Maybe a -> (a -> Maybe b) -> Maybe b
    link = flip chain
 -}

instance Monad [] where
    return = (: [])
    bind (a:ax) f = f a ++ bind ax f
    bind _ _ = []

newtype Gen a = Gen { unGen :: Seed -> (a , Seed) }

evalGen :: Gen a -> Seed -> a
evalGen ga s = fst (unGen ga s)
--bind :: m a -> (a -> m b) -> m b
instance Monad Gen where
    --bind ga f = Gen (uncurry (unGen . f) . unGen ga)
    bind ma f = Gen { unGen=(\s -> let (a, s') = unGen ma s in unGen (f a) s') }
    --bind ga f = Gen { unGen=(\s' -> unGen ((f.fst.(unGen ga)) $ s') s') }
    return a = Gen (\s -> (a,s))

--Revisiting Other Generic Functions
liftM2 :: (Monad m) => (a -> b -> c) -> m a -> m b -> m c
liftM2 = y
{-  return :: a -> m a
    bind :: m a -> (a -> m b) -> m b -}
----set1
sequence :: [Gen a] -> Gen [a]
sequence (a:ga) = bind a (\y -> bind (sequence ga) (\x ->return (y:x)) )
sequence _ = return []

----set2
(=<<) :: (a -> Maybe b) -> Maybe a -> Maybe b
(=<<) = flip bind
join :: Maybe (Maybe a) -> Maybe a
join a = bind a id
{- 
y :: Monad m => (a -> b -> c) -> m a -> m b -> m c
y f ma mb = bind ma (\a -> bind mb (return.f a))
 -}
----set3
liftM3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
liftM3 f ax bx cx =  bind ax (\a -> bind bx (\b -> bind cx (return.f a b)))
ap :: [a -> b] -> [a] -> [b]
ap [] _ = []
ap _ [] = []
ap fx ax = bind fx (\f -> bind ax (return.f $))

--Using the abstraction