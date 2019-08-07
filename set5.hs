{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set5 where

import MCPrelude

import Set2 (
    Maybe(..),
    headMay,
    tailMay,
    lookupMay,
    divMay,
    maximumMay,
    minimumMay
    )
import Set3 (Card(..))    
--Do Notation

--Do Notation â€“ operators
class Monad m where
    (>>=) :: m a -> (a -> m b) -> m b
    return :: a -> m a

    fail :: String -> m a
    fail = undefined

--Do Notation â€“ Set 1
newtype Gen a = Gen { unGen :: Seed -> (a , Seed) }

evalGen :: Gen a -> Seed -> a
evalGen ga s = fst (unGen ga s)

instance Monad Gen where
    (>>=) ma f = Gen { unGen=(\s -> let (a, s') = unGen ma s in unGen (f a) s') }
    return a = Gen (\s -> (a,s))

makeRandom :: Gen Integer
makeRandom = Gen(rand)

fiveRands :: Gen [Integer]
fiveRands = do
    g1 <- makeRandom
    g2 <- makeRandom
    g3 <- makeRandom
    g4 <- makeRandom
    g5 <- makeRandom
    return [g1,g2,g3,g4,g5]

randLetter :: Gen Char
--randLetter = makeRandom >>= (return.toLetter)
randLetter = do
    g <- makeRandom
    return.toLetter $ g

randString3 :: Gen String
randString3 = do
    g1 <- randLetter
    g2 <- randLetter
    g3 <- randLetter
    return [g1,g2,g3]
----(unGen (generalPair makeRandom makeRandom) ) $ mkSeed 1
generalPair :: Gen a -> Gen b -> Gen (a, b)    
generalPair ga gb= do
    g1 <- ga
    g2 <- gb
    return (g1,g2)

--Do Notation – Set 2
instance Monad Maybe where
    return = Just
    (>>=) ma f = case ma of 
        Just a -> f a
        Nothing -> Nothing

queryGreek :: GreekData -> String -> Maybe Double
queryGreek gd key = do
    xs  <- lookupMay key gd
    tx  <- tailMay xs
    m   <- maximumMay tx
    h   <- headMay xs
    divMay (fromIntegral m) (fromIntegral h)

addSalaries :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries xs n1 n2 = do
    s1 <- lookupMay n1 xs
    s2 <- lookupMay n2 xs
    return (s1 + s2)

tailProd :: Num a => [a] -> Maybe a
tailProd ax = do
    ta <- tailMay ax
    return.product $ ta

tailSum :: Num a => [a] -> Maybe a
tailSum ax = do
    sa <- tailMay ax
    return.sum $ sa

tailMax :: Ord a => [a] -> Maybe a
tailMax ax = do
    ma <- tailMay ax
    maxa <- maximumMay $ ma
    return maxa

--Do Notation – Set 3
instance Monad [] where
    return = (: [])
    (>>=) (a:ax) f = f a ++ (>>=) ax f
    (>>=) _ _ = []

allPairs :: [a] -> [b] -> [(a,b)]
allPairs ax bx = do
    a <- ax
    b <- bx
    return (a,b)

allCards :: [Int] -> [String] -> [Card]
allCards ax bx = do
    a <- ax
    b <- bx
    return (Card a b)

allCombs3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allCombs3 f ax bx cx = do
    a <- ax
    b <- bx
    c <- cx
    return (f a b c)
