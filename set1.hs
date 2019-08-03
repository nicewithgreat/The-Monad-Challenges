{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set1 where

import MCPrelude

fiveRands :: [Integer]
fiveRands = loopRandsbynum 5 1

fiveproduct :: Integer
fiveproduct = foldl (*) 1 fiveRands

loopRandsbynum :: Integer -> Integer -> [Integer]
loopRandsbynum 0 _ = []
loopRandsbynum acc key = let (nextkey,seed) = rand $ mkSeed key
                    in nextkey : loopRandsbynum (acc-1) nextkey
