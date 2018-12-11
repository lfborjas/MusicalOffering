# A Musical Offering

Exploring Haskell, and Bach's "A musical offering", with [Euterpea](http://www.euterpea.com/).

The code in this little repository uses the [Literate Haskell](https://wiki.haskell.org/Literate_programming) style, and was created using [`haskell-mode`](https://wiki.haskell.org/Haskell-mode) in Emacs for easy interactivity.

## Usage

### Presentation

The presentation's slides are provided in a pdf in the `presentation` folder. But if you're crazy (or curious), you can [install Racket](https://download.racket-lang.org/) and run the `talk.rkt` file with `slideshow talk.rkt`.

### Code

You should be able to import the `MusicalOffering.CrabCanon.Performance` module and all functions will be available in a `ghci`. Try `play crabCanon` (requires Euterpea, see the setup section).

## Setup

To use Euterpea, you need Haskell and a synthesizer. I've had a good degree of success with [Simplesynth](http://notahat.com/simplesynth/).

* Install [Haskell Platform](https://www.haskell.org/platform/) (either the full or core versions). It downloads an installer so you should be able to just follow the instructions on screen.
* Install Euterpea: `cabal update; cabal install Euterpea`.
* If you have a synth and everything went well, you should be able to play your first note:

```sh
λ ~/birchbox/a_musical_offering/ ghci
GHCi, version 8.4.3: http://www.haskell.org/ghc/  :? for help
Prelude> import Euterpea
Prelude Euterpea> play $ d 4 qn
Prelude Euterpea> 
```

That should play a C. If you don't hear any sound, see the [official documentation](http://euterpea.com/download-and-installation/)

### Known issues

#### Can't install Euterpea: compilation error:

```sh
λ ~/birchbox/a_musical_offering/ cabal install Euterpea
clang: error: unknown argument: '-no-pie'
`gcc' failed in phase `C Compiler'. (Exit code: 1)
```

This seems to be an issue with newer versions of `gcc` and certain assumptions that Haskell Platform makes. Luckily, you can edit that, in the GHC settings (in my installation, that's in `/Library/Frameworks/GHC.framework/Versions/8.4.3-x86_64/usr/lib/ghc-8.4.3/settings`).

```diff
5c5
< , ("C compiler supports -no-pie","YES")
---
> , ("C compiler supports -no-pie","NO")
```

After making that change, `cabal install Euterpea` should succeed.



## Some references:

* [Free, albeit incomplete, copy of the Haskell School of Music](http://haskell.cs.yale.edu/wp-content/uploads/2015/03/HSoM.pdf)
* ["Why functional programming matters"](https://www.cs.kent.ac.uk/people/staff/dat/miranda/whyfp90.pdf)
* [Score for the "crab canon"](https://imslp.org/wiki/Musikalisches_Opfer,_BWV_1079_(Bach,_Johann_Sebastian))
* [A short study on the canons](http://jan.ucc.nau.edu/tas3/musoffcanons.html)
* [An article on the canons](http://www.ams.org/publicoutreach/feature-column/fcarc-canons)
* https://en.wikipedia.org/wiki/The_Musical_Offering#Musical_riddles
* A glimpse into Haskell's Standard Prelude: http://hackage.haskell.org/package/base-4.12.0.0/docs/src/GHC.List.html
