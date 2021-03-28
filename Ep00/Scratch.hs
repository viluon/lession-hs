module Ep00.Scratch
where

data Tone = C | D | E | F | G | A | H

(^) :: Tone -> Int -> (Tone, Int)
(^) = (,)

data Note = Note
  { note_duration :: Double
  , note_tone     :: Tone
  , note_octave   :: Int
  }

(𝅝), (𝅗𝅥), (♩), (♪), (𝅘𝅥𝅯) :: Tone -> Int -> Note
(𝅝) = Note   1
(𝅗𝅥) = Note $ 1 / 2
(♩) = Note $ 1 / 4
(♪) = Note $ 1 / 8
(𝅘𝅥𝅯) = Note $ 1 / 16
