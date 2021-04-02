{-# LANGUAGE EmptyCase, FlexibleInstances #-}
{-# OPTIONS_GHC -Wincomplete-patterns -Wunused-matches #-}

module Ep01.Main
where

{-
Část 0: Syntax datových typů
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-}

data Bol = Tru | Fals -- ≃ { Tru, Fals }

not :: Bol -> Bol
not b = case b of
          Tru  -> Fals
          Fals -> Tru

and :: Bol -> Bol -> Bol
and Tru Tru = Tru
and _    _    = Fals






instance Eq Bol where
  Tru  == Tru  = True
  Fals == Fals = True
  _     == _   = False

xor :: Bol -> Bol -> Bol
xor x y | x == y = Fals
xor _ _          = Tru







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

filtr _ [] = []
filtr p (x:xs) = case p x of
                    Tru  -> x : rest
                    Fals -> rest
  where rest = filtr p xs

over limit = filtr p people
  where p (MkPerson _ _ age) | age > limit = Tru
        p _                                = Fals

-- >>> over 40






data IntList' = IntNil | IntCons Int IntList'


data List a = Nil | Cons a (List a)
--        ^ typový parametr

type    IntList = List Int
type StringList = List String
type    Map k v = List (k, v)


{-
Část I: Algebraické datové typy
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-}

data Pair a b = MkPair a b           -- ≃ a × b
  deriving Show

p1 :: Pair (Pair Int Bool) String
p1 = MkPair (MkPair 1 True) "hello"
-- >>> p1

p2 = MkPair (MkPair 1 2) (MkPair 3 4)
-- >>> p2

-- >>> case p2 of MkPair (MkPair one two) (MkPair three four) -> show two






data OneOf a b = First a | Second b     -- ≃ a ⊕ b
  deriving Show

e1 :: OneOf String Int
e1 = Second 4

e2 :: OneOf String Int
e2 = First "failed :("

safeDiv :: Int -> Int -> OneOf String Int
safeDiv _ 0 = First "divide by 0"
safeDiv x y = Second (x `div` y)








data Unit = Unit         -- ≃ { Unit }

unitValues = [Unit]

-- >>> unitValues

bolValues = error "to-do"

-- >>> bolValues

pairUnitBolValues :: [Pair Unit Bol]
pairUnitBolValues = error "to-do"

-- >>> pairUnitBolValues


-- >>> length pairUnitBolValues == length bolValues










data Void                -- ≃ ∅
