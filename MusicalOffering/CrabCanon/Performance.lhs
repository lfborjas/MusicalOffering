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
>
> pentatonic = mkScale' [2,2,3,2]
> blackKeys  = pentatonic $ fs 4 qn

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
> staccato :: Music a -> Music a
> staccato (Prim (Note d p)) =
>   note (d/8) p :+: rest (7*d/8)

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
>               (staccato $ c 5 qn), (staccato $ g 4 qn),
>               (staccato $ ef 4 qn),
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

> staccatoAll :: Dur -> [Dur -> Music a] -> Music a
> staccatoAll d ns = line $ map (staccato' d) ns

The retrogade of this won't revert the sub (nested) lines created by
staccatoAll or by addDur, so it's not a true retrogade!
Wonder if a more specialized function of retrogade would leave the
staccatos alone but revert the other kinds of sub-lines .

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


Some more Bach
===============


Bach's Sinfonia 15 in B minor
https://imslp.org/wiki/Sinfonia_in_B_minor,_BWV_801_(Bach,_Johann_Sebastian)
(a fantastic rendition: https://www.youtube.com/watch?v=Ya1m3kI2YM0)

Note that the numerator in the second term in the `t` value
is a good control of speed: less than the default of 120 makes it slower, e.g.
The 9/16 in the first term is simply from the score.

Note that all instances C and F are meant to be sharp, as this is in the
key of B Minor

some auxiliary functions (uses list comprehensions, introduced in the next chapter):

> pedal :: Int -> Music a -> [Dur -> Music a] -> Music a
> pedal t (Prim (Note d p)) ms = line [ m d :+: times 2 (note d p) | m <- ms ]
> pedal2 = pedal 2
> walk :: Dur -> [Dur -> Music a] -> Music a
> walk d (a:b:c:_) = addDur d [a,b,c,b,a]
> pedal' :: Int -> Music a -> [Dur -> Music a] -> Music a
> pedal' t pn ms = (times t pn) :+: pedal t pn ms -- start with pedal
> pedal'2 = pedal' 2
> intersperse :: Music a -> [Dur -> Music a] -> Music a
> intersperse (Prim (Note d p)) ms = line [ (note d p) :+: m d | m <- ms ]
> cheapTrill :: Music Pitch -> Music Pitch
> cheapTrill (Prim (Note d p)) =
>   note (d/5) p :+: note (d/5) (trans 1 p) :+:
>   note (d/5) p :+: note (d/5) (trans 1 p) :+:
>   note (d/5) p

> sinfonia15 :: Music Pitch
> sinfonia15 = let t = (9/16) * (140/120)
>              in instrument Harpsichord
>                 (tempo t (bass :=: treble))
> treble = trebl1  :+: (trebl2 :=: trebl3) :+:
>          trebl4  :+: (trebl5 :=: trebl6) :+:
>          trebl7  :+: (trebl8 :=: trebl9) :+:
>          trebl10 :+:
>          (trebl11 :=: trebl12) :+: trebl13 :+:
>          (trebl14 :=: trebl15)
> bass   = bas1 :+:
>          bas2 :+: (bas3 :=: bas4) :+:
>          bas5 :+: (bas6 :=: bas7) :+:
>          bas8 :+:
>          bas9
> trebl1 = addDur sn [b 4, fs 4, fs 4,
>                      g 4, fs 4, fs 4,
>                      b 4, fs 4, fs 4,
>                      cs 5, fs 4, fs 4,
>                      g 5, fs 4, fs 4,
>                      cs 5, fs 4, fs 4] :+: -- bars 1-2
>          addDur tn [d 5,
>                     b 4, d 5, fs 5, a 5,
>                     fs 5,
>                     d 5, fs 5,
>                     d 5, b 4, d 5, b 4,
>                     fs 4, b 4, fs 4, d 4, fs 4, d 4] :+: -- bar 3
>          b 3 sn :+: times 3 snr :+: addDur sn [b 4, cs 5, d 5, cs 5, b 4] :+: -- bar 4
>          as 4 (den + sn) :+: addDur sn [b 4, cs 5, b 4, cs 5, as 4] :+: -- bar 5
>          addDur tn [b 4, fs 4, b 4, d 5, f 5, d 5,
>                     b 4, d 5, b 4, fs 4, b 4, fs 4,
>                     d 4, fs 4, d 4, b 3, d 4, b 4] -- bar 6
> trebl3 = d 5 (dqn + den + sn) :+: -- bar 7, 8.25
>          addDur sn [b 4, b 4,
>                     cs 5, b 4, b 4,
>                     e  5, b 4, b 4] :+: -- bar 8
>          cs 5 (dqn + den + sn) :+: -- bar 9, 10.25
>          addDur sn [a 4, a 4,
>                     b 4, a 4, a 4,
>                     d 5, a 4, a 4] :+: -- bar 10
>          addDur tn [b 4, d 5, b 4, g 4, b 4, g 4] :+:
>          e 4 den :+:
>          addDur tn [e 5, g 5, e 5, cs 5, e 5, cs 5] -- bar 11
> trebl2 = addDur sn [b 4, fs 4, fs 4,
>                     g 4, fs 4, fs 4,
>                     b 4, fs 4, fs 4] :+: -- bar 7
>          gs 4 (dqn + den) :+: -- bar 8
>          addDur sn [a 4, e 4, e 4,
>                     fs 4, e 4, e 4,
>                     a 4, e 4, e 4] :+: -- bar 9
>          fs 4 (dqn + den) :+: -- bar 10
>          g 4 sn :+: snr :+: snr :+: denr :+: denr -- bar 11
> trebl4 = addDur tn [a 4, cs 5, a 4, fs 4, a 4, fs 4] :+:
>          d 4 den :+:
>          addDur tn [d 5, fs 5, d 5, b 4, d 5, b 4] :+: -- bar 12
>          addDur tn [g 4, b 4, g 4, e 4, g 4, e 4,
>                     cs 4, e 4, cs 4, a 3, cs 4, a 3] :+:
>          addDur sn [a 4, fs 4, g 4] :+: -- bar 13
>          (fs 4 den :=: d 4 sn)
> bas1 = b 2 den :+: snr :+:
>        addDur sn [b 3, cs 4, d 4, cs 4, b 3] :+:
>        as 3 (den + sn) :+: -- bars 1-2
>        addDur sn [b 3, cs 4, b 3, cs 4, as 3] :+:
>        b 3 den :+: denr :+: denr :+: -- bar 3
>        addDur sn [b 3, fs 3, fs 3,
>                   g 3, fs 3, fs 3,
>                   b 3, fs 3, fs 3,
>                   cs 3, fs 3, fs 3,
>                   g 3, fs 3, fs 3,
>                   cs 3, fs 3, fs 3] :+: -- bars 4-5
>        addDur tn [d 4, b 3, d 4, fs 4, b 4, d 5,
>                   d 4, fs 4, d 4, b 3, fs 4, b 3,
>                   fs 3, b 3, fs 3, d 3, fs 3, d 3] :+: -- bar 6
>        b 2 den :+: snr :+: addDur sn [b 3, cs 4, d 4, cs 4, b 3] :+: -- bar 7
>        e 4 den :+: snr :+: addDur sn [e 3, fs 3, gs 3, fs 3, e 3] :+: -- bar 8
>        a 3 den :+: snr :+: addDur sn [a 2, b 2, cs 3, b 2, a 2] :+: -- bar 9
>        d 3 den :+: snr :+: addDur sn [d 2, e 2, fs 2, e 2, d 2] :+: -- bar 10
>        g 2 den :+:
>        addDur tn [g 3, b 3, g 3, e 3, g 3, e 3,
>                   cs 3, e 3, cs 3, a 2, cs 3, a 2] :+: -- bar 11
>        fs 2 den :+: addDur tn [fs 3, a 3, fs 3, d 3, fs 3, d 3,
>                                b 2, d 3, b 2, g 2, b 2, g 2] :+: -- bar 12
>        e 2 den  :+: addDur tn [e 3, g 3, e 3, cs 3, e 3, cs 3,
>                                a 2, cs 3, a 2, fs 2, a 2, fs 2] -- bar 13
> ---- Defining them more closely together since bar 14:
> trebl6 = fs 4 den :+: denr :+: denr :+: -- bar 14
>          times 3 (denr) :+: -- bar 15 (the score has it as wnr
>          times 3 (denr) -- bar 16     but it was delayed too long)
> trebl5 = pedal2 (a 3 sn) [d 4, b 3, d 4] :+: -- bar 14
>          pedal2 (a 3 sn) [e 4, b 3, e 4] :+: -- bar 15
>          addDur tn [fs 4, d 4, a 4, d 5, b 4,
>                     gs 4, b 4, gs 4, e 4, gs 4, e 4] :+:
>          addDur tn [b 3, e 4, b 3, gs 3, b 3, gs 3] -- bar 16
> bas2 = d 2 den :+: snr :+: walk sn [d 2, e 2, f 2] :+: -- bar 14
>        cs 2 den :+: snr :+: walk sn [a 2, b 2, cs 3] :+: -- bar 15
>        d 3 (den + sn) :+: addDur sn [e 3, fs 3, e 3, fs 3, gs 3, d 3] -- bar 16
> trebl7 = pedal2 (e 4 sn) [a 4, fs 4, a 4] :+: -- bar 17
>          pedal2 (e 4 sn) [b 4, fs 4, b 4] :+: -- bar 18
>          addDur tn [cs 5, a 4, cs 5, e 5, a 5, fs 5,
>                     ds 5, fs 5, ds 5, b 4, ds 5, b 4,
>                     fs 4, b 4, fs 4, ds 4, fs 4, ds 4] -- bar 19
> bas4 = e 3 den :+: snr :+: walk sn [a 3, b 3, cs 4] :+: -- bar 17
>        gs 3 (den + sn) :+: addDur sn [a 3, b 3, a 3, b 3, gs 3] :+: -- bar 18
>        a 3  (den + sn) :+: addDur sn [b 3, c 4, b 3, c 4, a 3] -- bar 19
> bas3 = cs 3 den :+: denr :+: denr :+: -- bar 17
>        e  3 den :+: denr :+: denr :+: --bar 18
>        a  2 den :+: fs 3 dqn -- bar 19
> --- something may be weird with the times starting in bar 20?
> trebl9 = e 4 den :+: denr :+: g 4 (den + sn) :+: -- bar 20
>          pedal'2 (e 4 sn) [fs 4, a 4] :+: -- bar 21
>          fs 4 (dqn + den + sn) :+: -- bar 22 - 23.025
>          pedal'2 (d 4 sn) [e 4, g 4] :+: -- bar 23
>          e 4 (dqn + den + sn) -- bar 24
> trebl8 = pedal2 (b 3 sn) [e 4, c 4, e 4] :+: -- bar 20
>          cs 4 (dqn + den + sn) :+: -- bar 21, 22.025
>          pedal'2 (a 3 sn) [b 3, d 4] :+: -- bar 22
>          b  3 (dqn + den + sn) :+: -- bar 23,24.025
>          pedal'2 (gs 3 sn) [a 3, c 4] -- bar 24
> bas5   = g 3 den :+: snr :+: walk sn [e 3, fs 3, g 3] :+: -- bar 20
>          a 3 den :+: snr :+: walk sn [a 2, b 2, cs 3] :+: -- bar 21
>          d 3 den :+: snr :+: walk sn [d 3, e 3, fs 3] :+: -- bar 22
>          g 3 den :+: snr :+: walk sn [g 3, fs 3, e 3] :+: -- bar 23
>          cs 3 den :+: snr :+: walk sn [cs 3, b 2, as 2] -- bar 24
> -- final stretch: bar 26 may be suffering from insufficient
> -- silences in the bass line: seems to not wait enough?
> trebl10 = pedal'2 (cs 4 sn) [d 4, fs 4] :+: -- bar 25
>           intersperse (d 4 tn) [b 3, fs 4, fs 4] :+:
>           intersperse (b 4 tn) [fs 4, d 5, d  5] :+: 
>           (g 5 den) :+: -- bar 26
>           intersperse (cs 4 tn) [a 3, e 4,  d 4] :+:
>           intersperse (a  4 tn) [e 4, cs 5, cs 5] :+:
>           (fs 5 den) :+: -- bar 27
>           intersperse (b 3 tn)  [g 3, d 4, d 4] :+:
>           intersperse (g 4 tn)  [d 4, b 4, b 4] :+:
>           (e 5 (den + sn)) :+: -- bar 28
>           pedal'2 (fs 4 sn) [g 4, e 5] -- bar 29
> bas7    = as 3 (dqn + den) :+: -- bar 25
>           b 3 sn -- bar 26
> bas6    = (fs 2 den) :+: snr :+:
>           walk sn [fs 3, e 3, d 3] :+: -- bar 25
>           b 2 den :+:
>           intersperse (d 3 tn) [b 2, fs 3, fs 3] :+:
>           intersperse (b 3 tn) [g 3, e 4, e 4] :+: -- bar 26
>           a 4 den :+:
>           intersperse (cs 3 tn) [a 2, e 3, e 3] :+:
>           intersperse (a  3 tn) [fs 3, d 4, d 4] :+: -- bar 27
>           intersperse (g  4 tn) [b 4, d 4, d 4] :+:
>           intersperse (b  3 tn) [d 4, g 3, g 3] :+:
>           intersperse (e  3 tn) [g 3, cs 3, cs 3] :+: -- bar 28
>           as 2 (den + sn) :+:
>           addDur sn [b 2, cs 3, fs 2, b 2, as 2] -- bar 29
> trebl12 = d 5 (dqn + den + sn) :+: -- bar 30
>           pedal'2 (b 4 sn) [c 5, g 5] :+: -- bar 31
>           as 4 den :+: fs 5 (dqn + tn) -- bar 32
> trebl11 = pedal2 (b 3 sn) [f 4, d 4, f 4] :+: -- bar 30
>           g 4 (dqn + den) :+: -- bar 31
>           fs 4 den :+: as 4 dqn -- bar 32
> trebl13 = addDur tn [b 4, d 5, fs 5, b 5, fs 5] :+:
>           intersperse (d 5 tn) [fs 5, b 4, b 4] :+:
>           intersperse (fs 4 tn) [b 4, d 4, d 4] -- bar 33
> bas8    = b 2 den :+: denr :+: b 2 den :+: -- bar 30
>           e 3 (dqn + den + den + dqn) :+: -- bars 31-32
>           d 3 en :+: snr :+: denr :+: denr -- bar 33
> trebl15 = d 5 (dqn + den + sn) :+:  -- bar 34
>           pedal'2 (cs 5 sn) [gs 5, d 5] :+: -- bar 35
>           addDur sn [e 5, cs 5, cs 5] :+:
>           as 5 sn :+: walk sn [cs 5, d 5, e 5] :+: -- bar 36
>           addDur sn [fs 5, g 5, e 5] :+:
>           d 5 den :+: cheapTrill (cs 5 en) :+:
>           b 4 sn :+: -- bar 37
>           b 4 dhn -- bar 38, FIN
> trebl14 = e 4 (dqn + den + dqn + den) :+: -- bars 34-35
>           as 4 den :+: snr :+:
>           walk sn [as 4, b 4, cs 5] :+: -- bar 36
>           b 4 dqn :+: as 4 den :+: -- bar 37
>           b 4 dhn -- bar 38, FIN
> bas9    = addDur tn [b 3, g 3, b 3, d 4, g 4, d 4] :+:
>           intersperse (b 3 tn) [d 4, g 3, g 3] :+:
>           intersperse (d 3 tn) [g 3, b 2, b 2] :+: -- bar 34
>           es 2 den :+: denr :+: denr :+: -- bar 35
>           fs 2 den :+: snr :+:
>           walk sn [e 3, fs 3, g 3] :+: -- bar 36
>           d 3 en :+: e 3 sn :+:
>           fs 3 en :+: e 3 sn :+:
>           fs 3 en :+: fs 2 sn :+: -- bar 37
>           b 2 dhn -- bar 38, FIN

Another fun tune, perhaps for the presentation:
https://imslp.org/wiki/Toccata_and_Fugue_in_D_minor%2C_BWV_565_(Bach%2C_Johann_Sebastian)
Bach's tocatta and fugue in d-moll: https://www.youtube.com/watch?v=ho9rZjlsyYY
only the first few bars, however:


> mordent' :: Int -> Music Pitch -> Music Pitch
> mordent' n (Prim (Note d p)) =
>   (note (d/16) p) :+: (note (d/16) (trans n p)) :+: (note (14 * d/16) p)
> invMordent :: Music Pitch -> Music Pitch
> invMordent = mordent' (-1)
> mordent :: Music Pitch -> Music Pitch
> mordent = mordent' 1
> wait :: Int -> Dur -> Music Pitch -> Music Pitch
> wait n d p = (times n (rest d)) :+: p

> tocattaDMoll :: Music Pitch
> tocattaDMoll = let t = (4/4) * (20/120)
>                in instrument StringEnsemble1
>                   (tempo t ((manuale :=: pedale) :=:
>                             (manuale :=: pedale)))
> manuale = voice1a :=: voice2a 
> pedale = voice3a :+: 
>          voice3b :=: voice4a :+: (bigChord :=: coda)
> voice1a = invMordent (a 5 en) :+: tnr :+:
>           addDur sfn [g 5, f 5, e 5, d 5] :+: cs 5 tn :+:
>           d 5 sn
> voice2a = invMordent (a 4 en) :+: tnr :+:
>           addDur sfn [g 4, f 4, e 4, d 4] :+: cs 4 tn :+:
>           d 4 sn :+:
>           invMordent (a 4 en) :+: tnr :+:
>           addDur tn [e 4, f 4, cs 4, d 4] :+: rest (tn + sn)
> voice3a = rest (en + tn + sfn + sfn + sfn + sfn + tn + sn) :+:
>           invMordent (a 3 en) :+: tnr :+:
>           addDur tn [e 3, f 3, cs 3, d 3] :+: rest (tn + sn)
> voice3b = invMordent (a 3 en) :+: tnr :+:
>           addDur sfn [g 3, f 3, e 3, d 3] :+: cs 3 tn :+:
>           d 3 sn :+: rest (tn + sn)
> voice4a = invMordent (a 2 en) :+: tnr :+:
>           addDur sfn [g 2, f 2, e 2, d 2] :+: cs 2 tn :+:
>           d 2 sn :+: rest (tn + sn)
> bigChord = chord [c0, c1, c2, c3, c4, c5, c6]
> c0 = wait 0 sn (d 2 hn)
> c1 = wait 1 sn (cs 3  (qn + qn))
> c2 = (wait 2 sn (e  3 (qn + qn)))
> c3 = (wait 3 sn (g  3 (qn + qn + sn)))
> c4 = (wait 4 sn (b  3 (qn + qn)))
> c5 = (wait 5 sn (cs 4 (qn + qn))) 
> c6 = (wait 6 sn (e  4 (qn + qn)))
> coda = wait 6 sn (rest (qn + qn)) :+:
>        (d 3 qn :=: a 3 qn :=: d 4 qn) :=:
>        wait 1 sn (e 3 sn :+: fs 3 en)
