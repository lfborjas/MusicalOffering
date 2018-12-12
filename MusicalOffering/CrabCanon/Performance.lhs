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
> majChord _ = error "Only works for notes!"
>
> majChord' :: Music Pitch -> Music Pitch
> majChord' (Prim (Note d root@(pc, o))) =
>   note d root :=:
>   note d (trans 4 root) :=:
>   note d (trans 7 root) :=:
>   note d (pc, o+1)

Some higher order functions

> mystery1 :: [ Music a ] -> Music a
> mystery1 ns = foldl1 (:+:) ns
> mystery2 :: [ Music a ] -> Music a
> mystery2 ms = line $ map (transpose 12) ms

play $ mystery1 [d 4 qn, fs 4 qn, as 4 qn]
play $ mystery1 [d 4 qn, fs 4 qn] :+: mystery2 [d 4 qn, fs 4 qn]


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

Remember this tune?

> pcToQn :: PitchClass -> Music Pitch
> pcToQn pc = note qn (pc, 4)

With this, we reduce the melody to combine and reduce repetition:

> twinkle =
>   let m1 = line (map pcToQn [C, C, G, G, A, A]) :+: g 4 hn
>       m2 = line (map pcToQn [F, F, E, E, D, D]) :+: c 4 hn
>       m3 = line (map pcToQn [G, G, F, F, E, E]) :+: d 4 hn
>   in line [m1, m2, m3, m3, m1, m2]

Let's throw a couple of these together:

play $ twinkle :=: ((times 2 (rest hn)) :+: twinkle)


> canon :: (Int, Dur) -> [InstrumentName] -> Music a -> Music a
> canon (voices, delay) instruments mel =
>   let range  n = take n [0..]
>       instr  n = instruments !! (n `mod` length instruments)
>       offset dur m n = times n (rest dur) :+: m
>       voice dur m n = instrument (instr n) (offset dur m n)
>   in chord $ map (voice delay mel) (range voices)
>
> canon' :: (Int, Dur) -> Music a -> Music a
> canon' (voices, delay) mel =
>   let range n = take n [0..]
>       offset dur m n = times n
>                        (rest dur) :+: m
>   in chord $ map
>      (offset delay mel)
>      (range voices)

> shortFrereJacques :: Music Pitch
> shortFrereJacques = line [g 4 qn, a 4 qn, b 4 qn, g 4 qn,
>                           b 4 qn, c 5 qn, d 5 hn,
>                           d 5 den, e 5 sn, d 5 en, c 5 en, b 4 qn, g 4 qn,
>                           g 4 qn, e 4 qn, g 4 hn]




Now for the actual performance, Bach's crab canon:





