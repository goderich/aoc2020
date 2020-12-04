#lang racket

(require megaparsack
         megaparsack/text
         data/monad
         data/applicative
         data/either)

(define (valid1? pass-str)
  (define passport (string-split pass-str #px"(\\s+|\n)"))
  (define fields (map (λ (s) (first (string-split s ":"))) passport))
  (for/and ((fld '("byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid")))
    (member fld fields)))

(define part1-list
  (filter valid1? (string-split (file->string "inputs/day04.txt") "\n\n")))

;; part 1

(length part1-list)

;; part 2

(define hgt/p
  (do (num <- integer/p)
      (hgt-unit <- (or/p (string/p "cm")
                         (string/p "in")))
      (pure (cons num hgt-unit))))

(define (valid-hgt? hgt)
  (define result
    (from-success #f (parse-string hgt/p hgt)))
  (match result
    ((cons num "cm") (>= 193 num 150))
    ((cons num "in") (>= 76 num 59))
    (_ #f)))

(define hcl/p
  (do (char/p #\#)
      (repeat/p 6
                (or/p digit/p
                      (char-between/p #\a #\f)))
      (pure 'ok)))

(define pid/p
  (do (repeat/p 9 digit/p)
      eof/p
      (pure 'ok)))

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

(count valid2? part1-list)
