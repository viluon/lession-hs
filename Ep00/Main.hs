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

n = 20

message :: String
message = if n <= 16 then "less than 17"
          else if n /= 20 then "not twenty"
          else "it's 20"

message' :: String -- stringy jsou jen seznamy charakterů
message' = ['F', 'L', 'P']




grades = ['A' .. 'F']







bar = f 4
  where
    -- typ funkcí je (->), narozdíl od matematického zápisu (f: ℝ -> ℝ)
    -- píšeme čtyřtečku
    f :: Double -> Double
--  ^ ^^ ^^^^^^ ^^ ^^^^^^ obor hodnot
--  | |  |      |
--  | |  |      funkce
--  | |  definiční obor
--  | "je typu"
--  název
    f x = x^2
--  ^ ^   ^^^ tělo funkce je zároveň návratová hodnota
--  | |
--  | parametr(y)
--  znovu název








isEven' :: Int -> Bool
isEven' 0 = True
isEven' n = if n < 0 then isEven' (-n)
            else          not (isEven' (n - 1))










isEven :: Int -> Bool
isEven 0         = True
isEven n | n < 0 = isEven (-n)
isEven n         = not (isEven (n - 1))
--     ^ volné proměnné na levé straně vážou konkrétní hodnoty










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
add' =  \x  -> (\y  -> x + y)

add :: Int -> Int -> Int
add x y = x + y

-- >>> add 3 (2 * 2)

-- >>> add 3 2 * 2

-- >>> 3 + 2 * 2

-- >>> ((add 3) 2) * 2

-- >>> add (3 2) * 2

-- >>> add 5 * 2

inc' :: Int -> Int
inc' = add 1

-- >>> inc' 0

-- >>> inc' -1

inc :: Int -> Int
inc = (+) 1

-- >>> inc 0

-- >>> inc 23_208

--             (((f a) b) c) d
-- Int -> (Int -> (Int -> (Int -> Int)))

curry'' :: ((a, b) -> c) -> (a -> (b -> c))
curry'' f = \x -> \y -> f(x, y)

curry'  :: ((a, b) -> c) -> a -> b -> c
curry'  f x y = f (x, y)



add'' = curry' plus

inc'' = add'' 1


-- n-tice se skládají ze staticky známého přirozeného čísla libovolných typů
type PixelRGB     = (Int, Int, Int)
type Point2D      = (Double, Double)
type Entry        = (String, Int)

type StringIntMap = [Entry]

mag :: Point2D -> Double
mag (x, y) = sqrt (x^2 + y^2)

{-
Část II: Část II
~~~~~~~~~~~~~~~~

-}

-- >>> "abc" ++ "def"

-- >>> [1, 2, 3] ++ [10, 20, 30]

rep :: String -> String
rep xs = xs ++ rep xs

lolol :: String
lolol = rep "lo"

-- >>> take 3 lolol

-- >>> take 11 lolol

-- >>> take 10 ("tro" ++ lolol)

-- e = ∑_{n ∈ [0, ∞)} 1/(n!)

recipFacts''' :: [Double]
recipFacts''' = [1, 1, 1 / 2, 1 / 6, 1 / 24, 1 / 120]

recipFacts'' :: [Double]
recipFacts'' = 1 : 1 : (1 / 2 : (1 / 6 : (1 / 24 : (1 / 120 : []))))

-- >>> exp 1

e'' = sum recipFacts''

-- >>> e''















recipFacts' :: [Double]
recipFacts' = map (\i -> 1 / fact i) [0..]
  where
    toDouble :: Int -> Double
    toDouble = fromIntegral

    fact :: Int -> Double
    fact = error "to-do"



-- >>> sum (take 10 recipFacts')

-- >>> exp 1





















recipFacts :: [Double]
recipFacts = go 1
  where
    go k = 1 : map (/ k) (go (k + 1))

--     seznam = 1 :    1  :  (1  :  (1  :  (1  :  (...))))
--          k = 1,     2,     3,     4,     5,     ...
-- jmenovatel =        1,     2,     3*2,   4*3*2, ...

-- 1 : 1 : 1/1  : 1/1  : 1/1  : ...
-- 1 : 1 : 1/2  : 1/2  : 1/2  : ...
-- 1 : 1 : 1/2  : 1/6  : 1/6  : ...
-- 1 : 1 : 1/2  : 1/6  : 1/24 : ...
-- ...

-- >>> length $ takeWhile (> 0) recipFacts

-- >>> sum $ takeWhile (> 0) recipFacts

-- >>> exp 1
