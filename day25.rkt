#lang racket

;; Day 25 has just a single puzzle, no part 2.
(require threading)

;; We read in the two public keys as integers.
(match-define (list card-public-key door-public-key)
  (~>> "inputs/day25.txt"
       (file->lines)
       (map string->number)))

;; This function finds the loop size given a public key.
(define (find-loop-size public-key)
  (define (loop acc loop-size)
    (if (= acc public-key)
        loop-size
        (loop (remainder (* acc 7) 20201227)
              (add1 loop-size))))
  (loop 1 0))

;; We use the above function to determine the loop sizes
;; on both the card and the door.
(define card-loop-size (find-loop-size card-public-key))
(define door-loop-size (find-loop-size door-public-key))

;; This function calculates a secret encryption key
;; according to the puzzle's rules.
(define (transform loop subject-number)
  (for/fold ((acc 1))
            ((_ loop))
    (remainder (* acc subject-number) 20201227)))

;; We calculate both encryption keys, which should be the same.
;; This is the final puzzle's answer.
(transform card-loop-size door-public-key)
(transform door-loop-size card-public-key)
