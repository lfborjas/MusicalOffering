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

;; Color reference: https://docs.racket-lang.org/draw/color-database___.html
;; and regexes: https://docs.racket-lang.org/reference/regexp.html
(define (syntax-color token)
  (define match (λ (p) (regexp-match p token)))
  (define color (cond [(match #px"[[:digit:]]")                 "ForestGreen"] ;; numbers
                      [(or (match #px"[[:upper:]][^[:space:]]")
                           (match #px":[^[:alpha:]]:"))         "SteelBlue"] ;; types and type constructors
                      [(match #rx"(let|in|case|where)")         "Orange"] ;; keywords
                      [(match #px"[[:alpha:]]")                 "DimGray"] ;; alphanumeric and common functions
                      [(match #px"[^[:alpha:]]")                "DarkGoldenrod"] ;; other symbols
                      [else #f]))
  (if color
      (colorize (tt token) color)
      (tt token)))

(define (haskell src)
  (define tokens (string-split src #:trim? #t))
  (define leading-space  (or (regexp-match #px"^\\s+" src) '("")))
  (apply para (cons (tt (car leading-space)) (map syntax-color tokens))))

;;; And now, the actual presentation:
;; slideshow reference: https://docs.racket-lang.org/slideshow/Creating_Slide_Presentations.html

(slide
 (t "A Musical Offering")
 (small (colorize (t "Composing functions composing music") "gray")))

(slide
 (para "Let's learn some music, and Haskell!"))

(slide
 (para "This is some haskell")
 (para (haskell "simple :: Int -> Int -> Int")
       (haskell "       in map a + b + c")))


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


