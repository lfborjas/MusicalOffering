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
  (define tokens (string-split src " " #:trim? #f))
  (apply para (map syntax-color tokens)))

;;; And now, the actual presentation:

(slide
 (t "A Musical Offering")
 (small (colorize (t "Composing functions composing music") "gray")))

(slide
 (para "Let's learn some music, and Haskell!"))

(slide
 (para "This is some haskell")
 (para (haskell "simple :: Int -> Int -> Int")
       (haskell "       in map a + b + c")))



