#lang racket

;; Here I use the Megaparsack library to read the lines. It is slower
;; than chopping them up manually, but more declarative and more readable.
(require megaparsack
         megaparsack/text
         data/monad
         data/applicative
         data/either)

;; This parser reads a line and extracts the two numbers, the letter
;; to be found, and the password as a list of chars. We put the values
;; into a list and extract them later with match-define.
(define constraints/p
  (do (num1 <- integer/p)
      (char/p #\-)
      (num2 <- integer/p)
      space/p
      (letter <- letter/p)
      (char/p #\:)
      space/p
      (password-chars <- (many/p letter/p))
      eof/p
      (pure (list num1 num2 letter password-chars))))

;; Part 1 is just checking if the letter count is within the defined limits.
(define (valid? line)
  (match-define (list from to letter password-chars)
    (from-success #f (parse-string constraints/p line)))
  (define letter-count (count (curry eq? letter) password-chars))
  (<= from letter-count to))

;; Answer to part 1.
(count valid? (file->lines "inputs/day02.txt"))

;; In part 2, we need to check if the letter appears in position 1 or 2, but
;; not both. For that we can use `xor` from the standard library.
(define (valid2? line)
  (match-define (list ind1 ind2 letter password-chars)
    (from-success #f (parse-string constraints/p line)))
  (define (valid-letter? i)
    (eq? letter (list-ref password-chars (sub1 i))))
  (xor (valid-letter? ind1)
       (valid-letter? ind2)))

;; Answer to part 2.
(count valid2? (file->lines "inputs/day02.txt"))
