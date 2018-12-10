> module CrabCanon.Performance where
> import Euterpea

Euterpea has a few types that help us express musical ideas, as well as helper functions.


When "reading" a score or composing new music, we can try to find patterns and express them with functions. A naïve
approach to the crab canon theme is to just transcribe the notes:

> mel :: Music Pitch
> mel = line [c 4 en, d 4 en, e 4 en, f 4 en]
