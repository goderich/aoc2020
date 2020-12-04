#lang racket

(define (valid1? pass-str)
  (define passport (string-split pass-str #px"(\\s+|\n)"))
  (define fields (map (λ (s) (first (string-split s ":"))) passport))
  (for/and ((fld '("byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid")))
    (member fld fields)))

;; part 1

(define part1-list
  (filter valid1? (string-split (file->string "inputs/day04.txt") "\n\n")))

(length part1-list)

;; part 2

(define (valid-hgt? hgt)
  (define num
    (string->number
     (list->string
      (takef (string->list hgt) char-numeric?))))
  (cond
    ((string-suffix? hgt "cm") (>= 193 num 150))
    ((string-suffix? hgt "in") (>= 76 num 59))
    (else #f)))

(define (valid-hcl? hcl)
  (define chars (string->list hcl))
  (and (eq? #\# (first chars))
       (for/and ((c (rest chars)))
         (or (char-numeric? c)
             (member c '(#\a #\b #\c #\d #\e #\f))))
       (= 7 (length chars))))

(define (valid2? str)
  (define passport (string-split str #px"(\\s+|\n)"))
  (define fields (map (λ (s) (string-split s ":")) passport))
  (for/and ((fld fields))
    (define value (second fld))
    (match (first fld)
      ("byr" (>= 2002 (string->number (second fld)) 1920))
      ("iyr" (>= 2020 (string->number (second fld)) 2010))
      ("eyr" (>= 2030 (string->number (second fld)) 2020))
      ("hgt" (valid-hgt? value))
      ("hcl" (valid-hcl? value))
      ("ecl" (member value '("amb" "blu" "brn" "gry" "grn" "hzl" "oth")))
      ("pid" (and (= 9 (string-length value))
                  (for/and ((c (in-string value))) (char-numeric? c))))
      ("cid" #t))))

(count valid2? part1-list)
