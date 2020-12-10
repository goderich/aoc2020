#lang racket

(require threading)

(define jolts
  (let* ((adaptors (map string->number (file->lines "inputs/day10.txt")))
         (max-joltage (+ 3 (apply max adaptors))))
    (~>> adaptors
         (cons 0)
         (cons max-joltage)
         (sort _ <))))

(define distances
  (map -
       (rest jolts)
       (drop-right jolts 1)))

(* (count (curry = 1) distances)
   (count (curry = 3) distances))

(define (consecutive-ones lst)
  (for/fold ((num-ones 0)
             (acc '())
             #:result (reverse acc))
            ((x lst))
    (cond
      ((= 1 x) (values (add1 num-ones) acc))
      ((= 0 num-ones) (values num-ones acc))
      (else (values 0 (cons num-ones acc))))))

(~>> distances
     (consecutive-ones)
     (map (λ (x) (add1 (apply + (range x)))))
     (apply *))
