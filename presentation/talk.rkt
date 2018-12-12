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
                        [(match #rx"(let|in|case|where|of|data|type)") "Orange"] ;; keywords
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

 (para (haskell "simple a b = a + b"
                "simple 1 1"
                "--2"))

 'next
 (para (haskell "l = 1:2:3:[]"
                "--[1,2,3]"
                "1:[2,3]"
                "--[1,2,3]"))
 
 'next
 (para (haskell "add2 [] = []"
                "add2 (x:xs) = (2 + x) : (add2 xs)"
                "add2 [1,2,3]"
                "--[3,4,5]")))

(slide
 #:title "A taste of Euterpea"

 (para (haskell "concertA = (A, 4)"
                "quarterNote = 1/4"
                "qnA = note quarterNote concertA"))

 'next
 (para (haskell "qnA' = a 4 qn"
                "play qnA'"))

 'next
 (para (haskell "doReMi = c 4 qn :+: d 4 qn :+: e 4 qn"
                "cMaj   = c 4 qn :=: e 4 qn :=: g 4 qn"
                "(:+:) (c 4 qn) (e 4 qn)"
                "play doReMi"
                "play cMaj"
                "play (doReMi :+: cMaj)")))


(slide
 #:title "A soupcon of Types"

 ; type synonyms
 (para (haskell "type Octave = Int"
                "type Pitch  = ( PitchClass, Octave )"
                "type Dur    = Rational"
                ;; algebraic data type: constructors on the right
                "data PitchClass = Cff | Cf | C | Dff | Cs | Df | Css | D --..."))

 'next
 (t "Haskell infers types, but we can tell it:")
 ; type annotations, though haskell mostly infers:
 (para (haskell "qn :: Dur"
                "qn = 1/4"
                "(A, 4) :: Pitch"
                "simple :: Int -> Int -> Int"
                "add2 :: [ Int ] -> [ Int ]"
                "note :: Dur -> Pitch -> Music Pitch")))

(slide
 #:title "Polymorphic types (!)"

 (para (haskell "data Primitive = Note Dur Pitch"
                "               | Rest Dur"))

 'next
 (t "We can think about these constructors as functions:")
 (para (haskell "Note :: Dur -> Pitch -> Primitive"
                "Rest :: Dur -> Primitive"
                "Note (1/4) (A, 4)"))
 
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

  (t "A fancier type of pattern matching")
  (para (haskell "majChord :: Music Pitch -> Music Pitch"
                 "majChord (Prim (Note d root)) = "
                 "  note d root :=:"
                 "  note d (trans 4 root) :=:"
                 "  note d (trans 7 root)")))

(slide
 #:title "A higher order"
 ; mention things like map and foldl/scanl, plus currying and composition?
 ; what about `$`
 )


(slide
 #:title "To infinity, and beyond"
 ; infinite sequences, list comprehensions
 )

#;
(slide
 #:title "So you want to write a canon?"
 'alts ;; iterative approach
 (list (list )))

(slide
 #:title "A more complex problem")


#|
Outline:
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


