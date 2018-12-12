> module MusicalOffering.CrabCanon.Performance where
> import Euterpea

Euterpea has a few types that help us express musical ideas, as well as helper functions.

> concertA = (A, 4)
> quarterNote = 1/4
> qnA :: Music Pitch
> qnA  = note quarterNote concertA
> qnA' = a 4 quarterNote

play qnA
play qnA'

> doReMi :: Music Pitch
> doReMi = c 4 qn :+: d 4 qn :+: e 4 qn
> cMaj   = c 4 qn :=: e 4 qn :=: g 4 qn


Using what we already know, plus the true power of pattern matching:

> majChord :: Music Pitch -> Music Pitch
> majChord (Prim (Note d root)) = note d root :=:
>                                 note d (trans 4 root) :=:
>                                 note d (trans 7 root)

Some higher order functions

> mystery1 :: [ Music a ] -> Music a
> mystery1 ns = foldl1 (:+:) ns
> mystery2 :: [ Music a ] -> Music a
> mystery2 ms = line $ map (transpose 12) ms

play $ mystery1 [d 4 qn, fs 4 qn, as 4 qn]
play $ line $ mystery2 [d 4 qn, fs 4 qn]

Partial lazyness:

> forever' :: Music x -> [ Music x ]
> forever' m = m : forever' m
> foreverSilence :: Dur -> [ Music x ]
> foreverSilence dur = forever' $ rest dur
> foreverA o dur = forever' $ a o dur
> foreverA4qn = foreverA 4 qn

line $ take 2 foreverA440
Prim (Note (1 % 4) (A,4)) :+: (Prim (Note (1 % 4) (A,4)) :+: Prim (Rest (0 % 1)))


take 2 hnSilences

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
