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
    (define color (cond [(or (match #px"[[:upper:]][^[:space:]]")
                             (match #px":[^[:alpha:][:digit:]]:")) "SteelBlue"] ;; types and type constructors
                        [(match #rx"--.*")                        "ForestGreen"] ;; comments
                        [(match #rx"(let|in|case|where|of)")      "Orange"] ;; keywords
                        [(match #px"[[:alpha:][:digit:]]")        "DimGray"] ;; alphanumeric and common functions
                        [(match #px"[^[:alpha:]]")                "DarkGoldenrod"] ;; other symbols
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
 #:title "Very Basic Haskell"

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
 (para "This is some haskell")
 (para (haskell "simple :: Int -> Int -> Int"
                "simple x = let a = b"
                "           in x :+: x")))


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


