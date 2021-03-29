{-# LANGUAGE NoImplicitPrelude, EmptyCase #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}

module Ep01.Main
where

import Prelude hiding (Bool(..), filter)
import qualified Prelude as Builtin (Bool(..))

{-
Část 0: Syntax datových typů
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-}

data Bool = True | False -- ≃ { True, False }

not :: Bool -> Bool
not b = case b of
          True  -> False
          False -> True

and :: Bool -> Bool -> Bool
and True True = True
and _    _    = False






instance Eq Bool where
  True  == True  = Builtin.True
  False == False = Builtin.False

xor :: Bool -> Bool -> Bool
xor x y | x == y = False
xor _ _          = True







data Person = MkPerson String String Int

instance Show Person where
  show (MkPerson name surname age) = name ++ " " ++ surname ++
                                      " (" ++ show age ++ ")"

people :: [Person]
people = [ MkPerson "Vincent" "Vega"      40
         , MkPerson "Jules"   "Winnfield" 46
         , MkPerson "Butch"   "Coolidge"  39
         , MkPerson "Winston" "Wolfe"     55
         ]

filter _ [] = []
filter p (x:xs) = case p x of
                    True  -> x : rest
                    False -> rest
  where rest = filter p xs

over limit = filter p people
  where p (MkPerson _ _ age) | age > limit = True
        p _                                = False

-- >>> over 40






data IntList' = IntNil | IntCons Int IntList'


data List a = Nil | Cons a (List a)
--        ^ pattern-matching na typ

type    IntList = List Int
type StringList = List String
type    Map k v = List (k, v)


{-
Část I: Algebraické datové typy
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-}

class Sized a where
  size :: a -> Int

entropy :: Sized a => a -> Float
entropy x = logBase 2 (fromIntegral (size x))

instance Sized Bool where
  size _ = 2

-- >>> entropy True









data Unit = Unit         -- ≃ { Unit }

instance Sized Unit where
  size Unit = 1

-- >>> entropy Unit








data Void                -- ≃ ∅

instance Sized Void where
  size v = 0


instance (Sized a, Sized b) => Sized (Either a b) where
  size (Left  x) = 2 * size x
  size (Right y) = 2 * size y

-- >>> size True

-- >>> size (Left Unit :: Either Unit Unit)

instance (Sized a, Sized b) => Sized (a, b) where
  size (x, y) = size x * size y

-- >>> entropy (True, True)


instance Sized a => Sized (List a) where
  size Nil         = 2
  size (Cons x xs) = 2 * size x * size xs

-- >>> entropy (Nil :: List Bool)

-- >>> size (Cons True Nil)

-- >>> entropy (Cons Unit (Cons Unit (Cons Unit Nil)))

s = Cons Unit
z = Nil

-- >>> entropy (s (s (s z)))
