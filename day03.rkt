#lang racket

;; Here we use a function for both parts. Since the value of "down" is 1
;; in all cases but one, we can have it as the default value.
(define (numtrees right (down 1))
  ;; We read in the list here, to separate it from the collision checks
  ;; in the second fold. If "down" equals 1, it returns the full list, but
  ;; if it's 2, it returns only the even lines. Then we can iterate over
  ;; the resulting list below without worrying about "down" any longer.
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

;; Answer to part 1.

(numtrees 3)

;; Answer to part 2.

(* (numtrees 1)
   (numtrees 3)
   (numtrees 5)
   (numtrees 7)
   (numtrees 1 2))
