module Ep00.Main
where

{-
Část 0: Základy Haskell syntaxe
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-}

-- komentář do konce řádku

{- komentář
přes několik
řádků -}

-- definice začínají nepovinnou typovou anotací, která patří
-- k celému symbolu (ne např. k jednotlivým argumentům funkce)
foo :: String
-- ... a pokračují několika rovnicemi (pro konstanty je jen jedna)
foo = "foo"

bar = f 4
  where
    -- typ funkcí je (->), narozdíl od matematického zápisu (f: ℝ -> ℝ) píšeme čtyřtečku
    f :: Double -> Double
--  ^ ^^ ^^^^^^ ^^ ^^^^^^ kodoména
--  | |  |      |
--  | |  |      funkce
--  | |  doména
--  | "je typu"
--  název
    f x = x^2
--  ^ ^   ^^^ tělo funkce
--  | |
--  | parametr(y)
--  znovu název

isEven :: Int -> Bool
isEven 0         = True
isEven n | n < 0 = isEven (-n)
isEven n         = not (isEven (n - 1))

-- typový alias, Unicode podpora
type CeléČíslo = Int

-- plus': ℤ × ℤ -> ℤ
plus' :: (CeléČíslo, CeléČíslo) -> CeléČíslo
plus' = plus -- reference na symbol definovaný níže v souboru

-- ↓ odvozené typy lze do zdrojového kódu doplnit kliknutím
four = 4

-- ↓ výrazy v komentářích s předponou >>> lze vyhodnotit kliknutím
-- >>> four + 5

{-

Část I: Currying
~~~~~~~~~~~~~~~~

-}

plus :: (Int, Int) -> Int
plus(x, y) = x + y

-- >>> plus(3, (2 * 2))

-- >>> plus(3, 2 * 2)

-- >>> plus(3, 2) * 2

add' :: Int -> (Int -> Int)
add' x = \y -> x + y

add :: Int -> Int -> Int
add x y = x + y

-- >>> add 3 (2 * 2)

-- >>> add 3 2 * 2

-- >>> ((add 3) 2) * 2

-- >>> add (3 2) * 2

inc' :: Int -> Int
inc' = add 1

-- >>> inc' 0

-- >>> inc' -1

inc :: Int -> Int
inc = (+) 1

-- >>> inc 0

-- >>> inc 23_208

{-
Část II: Část II
~~~~~~~~~~~~~~~~

-}

-- >>> "abc" ++ "def"

rep :: String -> String
rep xs = xs ++ rep xs

lolol :: String
lolol = rep "lo"

-- >>> take 3 lolol

-- >>> take 11 lolol

-- >>> take 10 ("tro" ++ lolol)

recipFacts''' :: [Double]
recipFacts''' = [1, 1, 1 / 2, 1 / 6, 1 / 24, 1 / 120]

recipFacts'' :: [Double]
recipFacts'' = 1 : 1 : 1 / 2 : 1 / 6 : 1 / 24 : 1 / 120 : []

recipFacts' :: [Double]
recipFacts' = map (\i -> 1 / fact i) [0..]
  where
    fact :: Int -> Double
    fact = error "to-do"

recipFacts :: [Double]
recipFacts = go 1
  where
    go k = 1 : map (/ k) (go (k + 1))

-- >>> sum $ takeWhile (> 0) recipFacts'''

-- >>> exp 1
