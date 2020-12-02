#lang racket/base

(require racket/file
         racket/string
         racket/list
         racket/function
         racket/bool)

;; part 1

(define (valid? line)
  (define inputs (string-split line))
  (define from (string->number (first (string-split (first inputs) "-"))))
  (define to (string->number (second (string-split (first inputs) "-"))))
  (define letter (car (string->list (second inputs))))
  (define password (third inputs))
  (define letter-count (count (curry eq? letter) (string->list password)))
  (<= from letter-count to))

(count valid? (file->lines "inputs/day02.txt"))

;; part 2

(define (valid2? line)
  (define inputs (string-split line))
  (define ind1 (string->number (first (string-split (first inputs) "-"))))
  (define ind2 (string->number (second (string-split (first inputs) "-"))))
  (define letter (car (string->list (second inputs))))
  (define pass-chars (string->list (third inputs)))
  (define (valid-letter? i)
    (eq? letter (list-ref pass-chars (sub1 i))))
  (xor (valid-letter? ind1)
       (valid-letter? ind2)))

(count valid2? (file->lines "inputs/day02.txt"))