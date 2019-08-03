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

{--
loopRandsbychar :: Integer -> Integer -> String
loopRandsbychar 0 _ = []
loopRandsbychar acc key =   let (nextkey,seed) = rand $ mkSeed key
                            in toLetter nextkey : loopRandsbychar (acc-1) nextkey
--}                        