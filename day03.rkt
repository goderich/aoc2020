#lang racket

(define (numtrees right (down 1))
  (define rows
    (for/list ((line (file->lines "inputs/day03.txt"))
               (y-coord (in-naturals))
               #:when (zero? (remainder y-coord down)))
      (string->list line)))
  (for/fold ((acc 0))
            ((row rows)
             (x-coord (in-range 0 +inf.0 right)))
    (if (eq? #\# (list-ref row (remainder x-coord (length row))))
        (add1 acc)
        acc)))

;; part 1

(numtrees 3)

;; part 2

(* (numtrees 1)
   (numtrees 3)
   (numtrees 5)
   (numtrees 7)
   (numtrees 1 2))
