#lang racket

;; This day is well suited for parser combinators. All the imports below
;; are needed for parser combinators from the Megaparsack library.
(require megaparsack
         megaparsack/text
         data/monad
         data/applicative
         data/either)

;; Part 1
;;
;; The first function only determines that each passport has all the
;; required fields.
(define (valid1? pass-str)
  (define passport (string-split pass-str #px"(\\s+|\n)"))
  (define fields (map (λ (s) (first (string-split s ":"))) passport))
  (for/and ((fld '("byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid")))
    (member fld fields)))

;; We read the input, throwing out the invalid passports.
(define passport-list
  (filter valid1? (string-split (file->string "inputs/day04.txt") "\n\n")))

;; Answer to part 1.
(length passport-list)

;; Part 2
;;
;; In part 2, I use parser combinators. They allow me to precisely define
;; the strings I'm parsing, extract values from them, and combine parsers.

;; Parser for height values.
(define hgt/p
  (do (num <- integer/p)
      (hgt-unit <- (or/p (string/p "cm")
                         (string/p "in")))
      eof/p
      (pure (cons num hgt-unit))))

;; This function returns #t iff the height value is correctly formatted
;; and within the accepted range.
(define (valid-hgt? hgt)
  (define result
    (from-success #f (parse-string hgt/p hgt)))
  (match result
    ((cons num "cm") (>= 193 num 150))
    ((cons num "in") (>= 76 num 59))
    (_ #f)))

;; Hair colour parser. Here we only care about the format and not the value.
;; If the format is correct, it returns (success 'ok), which we can then
;; test with the `success?` predicate.
(define hcl/p
  (do (char/p #\#)
      (repeat/p 6
                (or/p digit/p
                      (char-between/p #\a #\f)))
      eof/p
      (pure 'ok)))

;; PID parser. Here we only care about the format and not the value.
(define pid/p
  (do (repeat/p 9 digit/p)
      eof/p
      (pure 'ok)))

;; This function checks if a passport is valid according to part 2 rules.
(define (valid2? str)
  (define passport (string-split str #px"(\\s+|\n)"))
  (define fields (map (λ (s) (string-split s ":")) passport))
  (for/and ((fld fields))
    (define value (second fld))
    (match (first fld)
      ("byr" (>= 2002 (string->number value) 1920))
      ("iyr" (>= 2020 (string->number value) 2010))
      ("eyr" (>= 2030 (string->number value) 2020))
      ("hgt" (valid-hgt? value))
      ("hcl" (success? (parse-string hcl/p value)))
      ("ecl" (member value '("amb" "blu" "brn" "gry" "grn" "hzl" "oth")))
      ("pid" (success? (parse-string pid/p value)))
      ("cid" #t))))

;; Answer to part 2.
(count valid2? passport-list)
