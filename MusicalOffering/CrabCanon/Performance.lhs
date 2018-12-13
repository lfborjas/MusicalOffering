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
> cMaj''   = c 4 qn :=: e 4 qn :=: g 4 qn


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
>
> mkScale' :: [Int] -> Music Pitch -> [Music Pitch]
> mkScale' ints (Prim (Note d p)) = map (note qn . pitch) $
>                                   scanl (+) (absPitch p) ints
>
> mkScale :: [Int] -> Music Pitch -> [Music Pitch]
> mkScale ints (Prim (Note d p)) = map (note qn . pitch) $
>                                  scanl (+) (absPitch p) (cycle ints)
>
> mkChord :: [Music Pitch] -> [Int] -> Music Pitch
> mkChord scale degrees =
>   chord $
>   map ((scale!!) . (subtract 1)) degrees

Where:
https://github.com/Euterpea/Euterpea/blob/6635e483cf80ec8ae67613c40e8d61e475f4742d/Euterpea/Music/Note/Music.hs#L134

scanl (+) 4 [2,1,2]

λ> cMaj = mkScale [2,2,1,2,2,2,1] (c 4 qn)
λ> mkChord cMaj [1,3,5,8]
Prim (Note (1 % 4) (C,4)) :=: (Prim (Note (1 % 4) (E,4)) :=: (Prim (Note (1 % 4) (G,4)) :=: (Prim (Note (1 % 4) (C,5)) :=: Prim (Rest (0 % 1)))))
λ> play $ mkChord cMaj [1,3,5,8]
λ> play $ mkChord cMaj [1,3,5,8,9]
λ> play $ mkChord cMaj [1,3,5,8,9,11]
λ> play $ line $ take 8 cMaj


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


> canon' :: (Int, Dur) -> [InstrumentName] -> Music a -> Music a
> canon' (voices, delay) instruments mel =
>   let range  n = take n [0..]
>       instr  n = instruments !! (n `mod` length instruments)
>       offset dur m n = times n (rest dur) :+: m
>       voice dur m n = instrument (instr n) (offset dur m n)
>   in chord $ map (voice delay mel) (range voices)
>
> canon :: (Int, Dur) -> Music a -> Music a
> canon (voices, delay) mel =
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


play $ canon' (2, hn) shortFrereJacques


Now for the actual performance, Bach's crab canon:
There's also page 5 of this: https://people.smp.uq.edu.au/PhilipPollett/talks/UQ2006/talk.pdf
in addition to the score we have elsewhere

First, a helper function to transcribe and perform


> lineToList' :: Music a -> [Music a]
> lineToList' (Prim (Rest 0)) = []
> lineToList' (n :+: ns) = n : lineToList' ns
> lineToList' _ = error "Need to provide a line!"
> retrograde :: Music Pitch -> Music Pitch
> retrograde = line . reverse . lineToList'

had an error before:
staccato d n = n (d/8) 
Performance.lhs:125:18-24: error: …
    • Couldn't match expected type ‘Dur -> Music a’
                  with actual type ‘[Dur -> Music a]’
    • The function ‘n’ is applied to one argument,
      but its type ‘[Dur -> Music a]’ has none
      In the expression: n (d / 8)
      In an equation for ‘staccato’: staccato d n = n (d / 8)
    • Relevant bindings include
        n :: [Dur -> Music a]
          (bound at /Users/luis.borjas/birchbox/a_musical_offering/MusicalOffering/CrabCanon/Performance.lhs:125:14)
        staccato :: Dur -> [Dur -> Music a] -> Music a
          (bound at /Users/luis.borjas/birchbox/a_musical_offering/MusicalOffering/CrabCanon/Performance.lhs:125:3)
    |
Compilation failed.


play $ tempo (2) crabTheme

> crabTheme :: Music Pitch
> crabTheme = line $
>             [rest 0, c 4 hn, ef 4 hn, g 4 hn, af 4 hn, b 3 hn,
>               qnr,
>               g  4 (qn + qn) , fs 4 hn ,
>               f  4 (qn + qn) , e  4 hn ,
>               ef 4 (qn + qn) , d  4 qn ,
>               df 4 qn , c 4 qn , b 3 qn ,
>               g  3 qn , c 4 qn , f 4 qn ,
>               ef 4 hn , d 4 hn , c 4 hn ,
>               ef 4 hn ,
>               g  4 en, f  4 en, g 4 en, c 5 en,
>               g  4 en, ef 4 en, d 4 en, ef 4 en,
>               f  4 en, g 4 en, a 4 en, b 4 en,
>               c  5 en, ef 4 en, f 4 en, g 4 en,
>               af 4 en, d 4 en, ef 4 en, f 4 en,
>               g  4 en, f 4 en, ef 4 en, d 4 en,
>               ef 4 en, f 4 en, g 4 en, af 4 en,
>               bf 4 en, af 4 en, g 4 en, f 4 en,
>               g  4 en, a 4 en, bf 4 en, c 5 en,
>               df 5 en, bf 4 en, af 4 en, g 4 en,
>               a  4 en, b 4 en, c 5 en, d 5 en,
>               ef 5 en, c 5 en, bf 4 en, a 4 en,
>               b  4 en, c 5 en, d 5 en, ef 5 en,
>               f  5 en, d 5 en, g 4 en, d 5 en,
>               c  5 en, d 5 en, ef 5 en, f 5 en,
>               ef 5 en, d 5 en, c 5 en, b 4 en,
>               c 5 qn, g 4 qn,
>               ef 4 qn,
>               c 4 qn , rest 0]
 
now we can play it:

> crabCanon :: Music Pitch
> crabCanon =
>   instrument Harpsichord $
>   crabTheme :=:
>   retrograde crabTheme
> 
> fastCanon :: Music Pitch
> fastCanon =
>   instrument Harpsichord $
>   tempo 2 $
>   crabTheme :=:
>   retrograde crabTheme
>
> fastTheme = tempo 3 crabTheme

play $ tempo (2) crabTheme
play $ (instrument Harpsichord crabTheme) :=: (instrument Harpsichord (retrograde $ crabTheme))

With some fanciness:

> addDur :: Dur -> [Dur -> Music a] -> Music a
> addDur d ns = line [ n d | n <- ns]
> staccato' :: Dur -> (Dur -> Music a) -> Music a
> staccato' d n = n (d/8) :+: rest (7*d/8)
> staccato :: Music a -> Music a
> staccato (Prim (Note d p)) =
>   note (d/8) p :+: rest (7*d/8)
> staccatoAll :: Dur -> [Dur -> Music a] -> Music a
> staccatoAll d ns = line $ map (staccato' d) ns

> crabTheme' = rest 0 :+:
>              addDur hn [c 4, ef 4, g 4, af 4, b 3] :+:
>              qnr :+:
>              g  4 (qn + qn) :+: fs 4 hn :+:
>              f  4 (qn + qn) :+: e  4 hn :+:
>              ef 4 (qn + qn) :+: d  4 qn :+:
>              df 4 qn :+: c 4 qn :+: b 3 qn :+:
>              g  3 qn :+: c 4 qn :+: f 4 qn :+:
>              ef 4 hn :+: d 4 hn :+: c 4 hn :+:
>              ef 4 hn :+:
>              addDur en [g  4, f  4, g 4, c 5,
>                         g  4, ef 4, d 4, ef 4,
>                         f  4, g 4, a 4, b 4,
>                         c  5, ef 4, f 4, g 4,
>                         af 4, d 4, ef 4, f 4,
>                         g  4, f 4, ef 4, d 4,
>                         ef 4, f 4, g 4, af 4,
>                         bf 4, af 4, g 4, f 4,
>                         g  4, a 4, bf 4, c 5,
>                         df 5, bf 4, af 4, g 4,
>                         a  4, b 4, c 5, d 5,
>                         ef 5, c 5, bf 4, a 4,
>                         b  4, c 5, d 5, ef 5,
>                         f  5, d 5, g 4, d 5,
>                         c  5, d 5, ef 5, f 5,
>                         ef 5, d 5, c 5, b 4] :+:
>              staccatoAll qn [c 5, g 4, ef 4] :+:
>              c 4 qn :+: rest 0
