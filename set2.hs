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