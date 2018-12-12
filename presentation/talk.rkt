#lang slideshow

(require slideshow/text
         slideshow/code)

#|
Yep, this presentation about Haskell is written in a Lisp.
Check out Racket's slideshow, it's neat!
https://docs.racket-lang.org/slideshow/index.html

To run it, if you have racket installed, just do:

$ slideshow talk.rkt
|#

;;; First, some helper functions:

;; Takes any number of lines Haskell code, as strings, and applies syntax
;; highlighting to each. Preserves leading whitespace.
;; Color reference: https://docs.racket-lang.org/draw/color-database___.html
;; and regexes: https://docs.racket-lang.org/reference/regexp.html
(define (haskell . lines)
  (define (syntax-color token)
    (define match (λ (p) (regexp-match p token)))
    (define color (cond [(or (match #px"^[[:upper:]][^[:space:]]*")
                             (match #px":[^[:alpha:][:digit:]]:"))     "SteelBlue"] ;; types and type constructors
                        [(match #rx"--.*")                             "ForestGreen"] ;; comments
                        [(match #rx"^(let|in|case|where|of|data|type)$") "Orange"] ;; keywords
                        [(match #px"[[:alpha:][:digit:]]")             "DimGray"] ;; alphanumeric and common functions
                        [(match #px"[^[:alpha:]]")                     "DarkGoldenrod"] ;; other symbols
                        [else #f]))
    (if color
        (colorize (tt token) color)
        (tt token)))
  (define tokens (λ (line) (string-split line #:trim? #t)))
  (define leading-space (λ (line) (or (regexp-match #px"^\\s+" line) '(""))))
  (define highlight (λ (line)
                      (apply para (cons (tt (car (leading-space line)))
                                        (map syntax-color (tokens line))))))
  (map highlight lines))

;;; And now, the actual presentation:
;; slideshow reference: https://docs.racket-lang.org/slideshow/Creating_Slide_Presentations.html

(slide
 (t "A Musical Offering")
 (small (colorize (t "Composing functions composing music") "gray")))

(slide
 #:title "A Little Haskell"

 (t "Functions!")
 (para (haskell "simple a b = a + b"
                "simple 1 1 --2"))

 'next
 (t "Lists! Cons-es!")
 (para (haskell "l = 1:2:3:[]"
                "--[1,2,3]"
                "1:[2,3]"))
 
 'next
 (t "Pattern matching!")
 (para (haskell "add2 [] = []"
                "add2 (x:xs) = (2 + x) : (add2 xs)"
                "add2 [1,2,3]"
                "--[3,4,5]")))

(slide
 #:title "A taste of Euterpea"

 (t "What's a note?")
 (para (haskell "concertA = (A, 4) --Tuple"
                "quarterNote = 1/4 --Rational"
                "qnA = note quarterNote concertA"))

 'next
 (para (haskell "qnA' = a 4 qn"
                "play qnA'"))

 'next
 (t "What are notes good for?")
 (para (haskell "doReMi = c 4 qn :+: d 4 qn :+: e 4 qn"
                "cMaj   = c 4 qn :=: e 4 qn :=: g 4 qn"
                "play doReMi"
                "play cMaj"
                "play (doReMi :+: cMaj)")))


(slide
 #:title "Types, types, types"

 ; type synonyms
 (t "Type synonyms:")
 (para (haskell "type Octave = Int"
                "type Pitch  = ( PitchClass, Octave )"
                "type Dur    = Rational"
                ;; algebraic data type: constructors on the right
                "data PitchClass = Cff | Cf | C | Dff | Cs | Df | Css | D --..."))

 'next
 (t "Haskell infers types, but we can tell it too:")
 ; type annotations, though haskell mostly infers:
 (para (haskell "qn :: Dur"
                "qn = 1/4"
                "(A, 4) :: Pitch"
                "add2 :: [ Int ] -> [ Int ]"
                "note :: Dur -> Pitch -> Music Pitch"
                "a,b,c,d,e,f,g :: Octave -> Dur -> Music Pitch")))

(slide
 #:title "A Type of Music"

 (t "Basic units of music:")
 (para (haskell "data Primitive = Note Dur Pitch"
                "               | Rest Dur"))

 'next
 (t "We can think about these 'constructors' as functions:")
 (para (haskell "Note :: Dur -> Pitch -> Primitive"
                "Rest :: Dur -> Primitive"))
 
 'next
 (t "Okay definition... but notes are more than pitches (percussion, volume, etc.)")

 'next
 ; what about other information about notes? (what if it has no pitch
 ; or if we want to also know about its volume or dynamics?)
 ; a is a type variable
 (para (haskell "data Primitive a = Note Dur a"
                "                 | Rest Dur"))
 (t "Which looks like:")
 (para (haskell "Note :: Dur -> a -> Primitive a"
                "Rest :: Primitive a")))

(slide
 #:title "A fancier (recursive!) type"
 (t "Music is not just single notes")
 (para (haskell "data Music a = Prim ( Primitive a )"
                "             | Music a :+: Music a"
                "             | Music a :=: Music a"))
 'next
 (para (haskell "Prim  :: Primitive a -> Music a"
                "(:+:) :: Music a -> Music a -> Music a"
                "(:=:) :: Music a -> Music a -> Music a")))

(slide
 #:title "Reasoning with types"
  (para (haskell "line :: [ Music a ] -> Music a"
                 "line [] = rest 0"
                 "line (m:ms) = m :+: line ms"
                 "line [c 4 qn, e 4 qn, g 4 qn, c 5 qn]"
                 "chord :: [ Music a ] -> Music a"
                 "chord [c 4 qn, e 4 qn, g 4 qn, c 5 qn]"))


)

(slide
 #:title "Pattern matching, TNG"
 (t "A fancier type of pattern matching")
 'alts
 (list (list 
        (para (haskell "majChord :: Music Pitch -> Music Pitch"
                       "majChord (Prim (Note d root)) = "
                       "  note d root :=:"
                       "  note d (trans 4 root) :=:"
                       "  note d (trans 7 root)"
                       "majChord _ = error 'only works for notes!'")))
       (list
        (para (haskell "majChord :: Music Pitch -> Music Pitch"
                       "majChord (Prim (Note d r@(pc, oct))) = "
                       "  note d r :=:"
                       "  note d (trans 4 r) :=:"
                       "  note d (trans 7 r) :=:"
                       "  note d (pc, o+1))"
                       "majChord _ = error 'only works for notes!'")))))

(slide
 #:title "Transcending"
 ; mention things like map and foldl/scanl, plus currying and composition?
 ; what about `$`
 (t "Guess what these functions do:")
 (para (haskell "mystery1 :: [ Music a ] -> Music a"
                "mystery1 ns = foldl1 (:+:) ns"
                "mystery1 [d 4 qn, fs 4 qn]"
                "--Prim (Note (1 % 4) (D,4)) :+: Prim (Note (1 % 4) (Fs,4))"))
 'next
 (t "Yep, that's our good old line function, refactored!"))

(slide
 (t "How about:")
 (para (haskell "mystery2 :: [ Music a ] -> Music a"
                "mystery2 ms = line $ map (transpose 12) ms"
                "mystery1 [d 4 qn, fs 4 qn] :+: mystery2 [d 4 qn, fs 4 qn]"))

 'next
 (para "Noticed the" (tt "$") "syntactic sugar?")
 (para (haskell "line (map (transpose 12) ms)")))


(slide
 #:title "Haskell: Curry"

 (t "Partial application is the default:")
 (para (haskell "add a b = a + b"
                "add2 = add 2"
                "add2 40 --42"
                "(add 2) 40"))
 (t "Which comes in handy:")
 (para (haskell "line  = foldr1 (:+:)"
                "chord = foldr1 (:=:)")))

(slide
 #:title "Example: scales"
 (t "How would we build a scale?")
 'alts
 (list (list (t "What we want:")
             (para (haskell "mkScale [2,2,3,2] (fs 4 qn)"
                            "--Fs, Gs, As, Cs, Ds")))
       (list (t "In Music:")
             (bitmap "black_keys.png"))
       (list (t "In English:")
             (item "Take a list of intervals")
             (item "Get each abs pitch relative to the root")
             (item "Turn those abs pitches into notes")
             (item "Turn those notes into a line"))
       (list (t "In Haskell:")
             (para (haskell "mkScale :: [Int] -> Music Pitch -> Music Pitch"
                            "mkScale ints (Prim (Note d p)) ="
                            "  line $ "
                            "  map (note qn . pitch) $"
                            "  scanl (+) (absPitch p) ints")))
       (list (t "Which we can use to define stuff:")
             (para (haskell "pentatonic = mkScale [2,2,3,2]"
                            "pentatonic $ fs 4 qn")))))

(slide
 #:title "Haskell is lazy"

 (t "And lazy evaluation is also a default:")
 (para (haskell "forever :: Music x -> [ Music x ]"
                "forever m = m : forever m"
                "foreverA o dur = forever $ a o dur"
                "foreverA4qn = foreverA 4 qn"))

 (t "No explosions, until...")
 (para (haskell "play $ line $ take 2 foreverA4qn")))

(slide
 #:title "Let's use our knowledge for music!"
 (t "Remember this tune?")
 'alts
 (list (list 
        (bitmap "twinkle.png"))
       (list
        (bitmap "twinkle_annotated.png"))
       (list
        (para (haskell "pcToQn :: PitchClass -> Music Pitch"
                       "pcToQn pc = note qn (pc, 4)"))
        (para (haskell "twinkle ="
                      "  let m1 = line (map pcToQn [C,C,G,G,A,A]) :+: g 4 hn"
                      "      m2 = line (map pcToQn [F,F,E,E,D,D]) :+: c 4 hn"
                      "      m3 = line (map pcToQn [G,G,F,F,E,E]) :+: d 4 hn"
                      "  in line [m1, m2, m3, m3, m1, m2]")))))

(slide
 #:title "Playing with music"
 (para (haskell "times 0 m = rest 0; times n m = m :+: times (n - 1) m"
                "play $ twinkle :=: ((times 2 (rest hn)) :+: twinkle)"))

 (t "What if we want to generalize it?")
 (para (haskell "canon :: (Int, Dur) -> Music a -> Music a"
                "canon (2, hn) twinkle")))

(slide
 (t "A function for creating canons:")
 (para (haskell "canon :: (Int, Dur) -> Music a -> Music a"
                "canon (voices, delay) mel ="
                "  let range n = take n [0..]"
                "      wait d m n = times n (rest dur) :+: m"
                "  in chord $ map"
                "      (wait delay mel) (range voices)"
                ""))

 (t "With some interesting results: ")
 (para (haskell "play $ canon (2, hn) twinkle"
                "play $ canon (2, qn) twinkle"
                "play $ canon (2, en) twinkle")))

(slide
 #:title "A cooler puzzle"
 (t "Bach's 'Crab Canon'")
 (tt "Canon 1 a 2")
 (bitmap "canon_cancrizans.png"))

(slide
 #:title "Takeaways"
 (item "Rephrase problems in terms of existing solutions")
 (item "Find the glue in your language (in Haskell: lazy, partial, high-ordered, functions")
 (item "Programming can be a tool to approach art, too!"))


#|
Outline:
https://www.youtube.com/watch?v=36ykl2tJwZM
https://www.youtube.com/watch?v=xUHQ2ybTejU

* A tour of haskell basics
** Function application
** Basic syntax (precedence)
** Type annotations
** Local declarations
** Pattern matching
* A tour of Euterpea, and more haskell
* Playing some music
* But how? Polymorphic data types
* Some convenience functions
* Incremental problem solving
** How to write a simple melody
** How to write a simple canon
*** Unrolled: limited voices, instruments
*** List functions: deriving foldl
*** Partial application (currying)
*** List comprehension
*** Infinite sequences
*** All together: a more general canon
** Trying to solve Bach's crab canon.
* Conclusions:
** "Glue": things like higher order fns, partial application/lazy evaluation
   help define new solutions in terms of previous solutions.
** That's expressive, but also smart: phrase your problem in terms of already
   solved problems (the Birchbox Engineering way!)
** Maybe coding is like music: seeing patterns, putting them together.
|#


