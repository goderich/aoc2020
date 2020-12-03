#lang racket

(define (numtrees right (down 1))
  (for/fold ((acc 0)
             (x-coord 0)
             #:result acc)
            ((line (file->lines "inputs/day03.txt"))
             (y-coord (in-naturals))
             #:when (zero? (remainder y-coord down)))
    (define row (string->list line))
    (define tree?
      (eq? #\# (list-ref row (remainder x-coord (length row)))))
    (values
     (if tree? (add1 acc) acc)
     (+ right x-coord))))

;; part 1

(numtrees 3)

;; part 2

(* (numtrees 1)
   (numtrees 3)
   (numtrees 5)
   (numtrees 7)
   (numtrees 1 2))
