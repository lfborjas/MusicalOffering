> module MusicalOffering.CrabCanon.Performance where
> import Euterpea

Euterpea has a few types that help us express musical ideas, as well as helper functions.


When "reading" a score or composing new music, we can try to find patterns and express them with functions. A naïve
approach to the crab canon theme is to just transcribe the notes:

> mel :: Music Pitch
> mel = line [c 4 en, d 4 en, e 4 en, f 4 en]

General fns: notice that `reductions` is my version of `scanl` as used above,
and `take` and `repeat` are in the standard prelude:


> reductions :: (b -> a -> b) -> b -> [a] -> [b]
> reductions f init [] = init : [] -- turn a single value into a list
> reductions f init (x:xs) = init : (reductions f (f init x) xs)
> repeat' :: Num b => Ord b => a -> b -> [a]
> repeat' a times  = let r y 0 ys = ys
>                        r y n ys = r y (n-1) (y:ys)
>                    in r a times []
> repeatedly :: Int -> a -> [a]
> repeatedly n x = take n (repeat x)
> each :: (a -> b) -> [a] -> [b]
> each f [] = []
> each f (x:xs) = (f x):(each f xs)
> range :: Num a => Enum a => Int -> [a]
> range n = take n [0..]

> delay :: Music a -> Dur -> Int -> Music a
> delay mel delayVal delayTimes = let rests dv dt = line (repeatedly dt (rest dv))
>                                 in (rests delayVal delayTimes) :+: mel
> voices :: Music a -> Int -> Music a
> voices mel nVoices = foldl1 (:=:) (each (delay mel hn) (range (nVoices - 1)))
> canon :: Music a -> Int -> [InstrumentName] -> Music a
> canon mel nVoices instruments =
>   let chooseInstrument n = instruments !! (n `mod` (length instruments))
>       voice m dur n = instrument (chooseInstrument n) (delay m dur n)
>   in foldl1 (:=:) (each (voice mel hn) (range (nVoices - 1)))

> shortFrereJacques :: Music Pitch
> shortFrereJacques = line [g 4 qn, a 4 qn, b 4 qn, g 4 qn,
>                           b 4 qn, c 5 qn, d 5 hn,
>                           d 5 den, e 5 sn, d 5 en, c 5 en, b 4 qn, g 4 qn,
>                           g 4 qn, e 4 qn, g 4 hn]
