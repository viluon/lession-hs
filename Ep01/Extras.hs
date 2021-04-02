{-# LANGUAGE FlexibleInstances #-}
module Ep01.Extras
where

data Unit = Unit         -- ≃ { Unit }

data Void                -- ≃ ∅

data List a = Nil | Cons a (List a)

class Sized a where
  size :: a -> Int

entropy :: Sized a => a -> Float
entropy x = logBase 2 (fromIntegral (size x))

instance Sized Bool where
  size _ = 2

-- >>> entropy True

instance Sized Unit where
  size Unit = 1

-- >>> entropy Unit

instance Sized Void where
  size v = 0

instance (Sized a, Sized b) => Sized (Either a b) where
  size (Left x) = 2 * size x
  size (Right y) = 2 * size y

-- >>> size True

-- >>> size (Left Unit :: Either Unit Unit)

instance (Sized a, Sized b) => Sized (a, b) where
  size (x, y) = size x * size y

-- >>> entropy (True, True)

instance Sized a => Sized (List a) where
  size Nil = 2
  size (Cons x xs) = 2 * size x * size xs

-- >>> entropy (Nil :: List Bool)

-- >>> size (Cons True Nil)

-- >>> entropy (Cons Unit (Cons Unit (Cons Unit Nil)))

s = Cons Unit

z = Nil

-- >>> entropy (s (s (s z)))

data Empty a

data InnocentList f a = InnocentNil | InnocentCons a (f a)

instance Sized a => Sized (InnocentList Empty a) where
  size InnocentNil = 1
  size (InnocentCons x _) = size x
