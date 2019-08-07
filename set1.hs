{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set1 where

import MCPrelude

--Random Number Generation
fiveRands :: [Integer]
fiveRands = loopRandsbynum 5 1

fiveproduct :: Integer
fiveproduct = foldl (*) 1 fiveRands 

loopRandsbynum :: Integer -> Integer -> [Integer]
loopRandsbynum 0 _ = []
loopRandsbynum acc key =    let (nextkey,seed) = rand $ mkSeed key
                            in nextkey : loopRandsbynum (acc-1) nextkey

--Random Character Generation
randLetter :: Seed -> (Char , Seed)
randLetter seed =   let (key,nextseed) = rand seed
                    in ((toLetter key),nextseed)

----have not used the function(randLetter)                  
randString2 :: String
randString2 = foldr ((:).toLetter) [] $ loopRandsbynum 2 1

----I can't use the (unSeed) 
randString3 :: String
randString3 = loopRand toLetter 3 1

loopRand :: (Integer -> a) -> Integer -> Integer -> [a]
loopRand _ 0 _ = []
loopRand f acc key =    let (nextkey,seed) = rand $ mkSeed key
                        in  (f nextkey) : loopRand f (acc-1) nextkey

{--冗余
loopRandsbychar :: Integer -> Integer -> String
loopRandsbychar 0 _ = []
loopRandsbychar acc key =   let (nextkey,seed) = rand $ mkSeed key
                            in toLetter nextkey : loopRandsbychar (acc-1) nextkey
--}

--More Generators
----before
randbyfunction :: (Integer -> a) -> Integer -> a
randbyfunction f key =  let (nextkey,_) = rand $ mkSeed key
                        in  f nextkey

randEven1 :: Integer
randEven1 = randbyfunction (*2) 1

randOdd1 :: Integer
randOdd1 = randbyfunction (+1).(*2) $ 1

randTen1 :: Integer
randTen1 = randbyfunction (*10) 1

treeproduct1 :: Integer
treeproduct1 = randEven1 * randOdd1 * randTen1
----after
type Gen a = Seed -> (a , Seed)
generalA :: (a -> b) -> Gen a -> Gen b
generalA f gena =   (\(a , b) -> (f a , b)) . gena

randEven :: Gen Integer
randEven = generalA (*2) rand

randOdd :: Gen Integer
randOdd = generalA (+1) randEven

randTen :: Gen Integer
randTen =  generalA (*10) rand

treeproduct :: Integer
treeproduct =   let key = mkSeed 1
                in (fst.randEven $ key) * (fst.randOdd $ key) * (fst.randTen $ key)

--Generalizing Random Pairs
randPair :: Gen (Char , Integer)
randPair = (\(a,b) -> ((a , fst $ rand b) , snd $ rand b)) . randLetter

generalPair :: Gen a -> Gen b -> Gen(a,b)
generalPair randa randb = ( \(key,seed) -> ((key , fst $ randb seed) , snd $ randb seed) ) . randa

generalB :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
generalB f randa randb seed =   (f a b , seed2)
                        where   (a,seed1) = randa seed
                                (b,seed2) = randb seed1

generalPair2 = generalB (\a b -> (a , b))
--
--Generalizing Lists of Generators
repRandom :: [Gen a] -> Gen [a]
repRandom []  s = ([],s)
repRandom (g:gx) seed = (a:ax , ss)
                where   (a,s) = g seed
                        (ax,ss) = repRandom gx s

--Threading the random number state
getTwo :: Gen a -> (a -> Gen b) -> Gen b
getTwo ga f seed = f a s
            where (a,s) = ga seed
        
mkGen :: a -> Gen a
mkGen m = (\(a , b) -> (m , b)) . rand

--set4
generalB2 :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
generalB2 f ga gb = getTwo ga (\a-> getTwo gb (mkGen.f a))
--generalA :: (a -> b) -> Gen a -> Gen b
repRandom2 :: [Gen a] -> Gen [a]
repRandom2 (a:ga) = getTwo a ( \x -> generalA (\l -> (:) x l) (repRandom2 ga) )
repRandom2 _ = mkGen []

