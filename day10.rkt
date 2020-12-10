#lang racket

(require threading)

(define jolts
  (let* ((adaptors (map string->number (file->lines "inputs/day10.txt")))
         (max-joltage (+ 3 (apply max adaptors))))
    (~>> adaptors
         (cons 0)
         (cons max-joltage)
         (sort _ <))))

(define diffs
  (map -
       (rest jolts)
       (drop-right jolts 1)))

(* (count (curry = 1) diffs)
   (count (curry = 3) diffs))

(define (skippable? n)
  (define prev (list-ref jolts (sub1 n)))
  (define next (list-ref jolts (add1 n)))
  ((- next prev) . <= . 3))

(count skippable? (range 1 (sub1 (length jolts))))

;; '(1 (6 7 8) 17 18 26 27 (35 36 37) (45 46 47) 52 60 61 66 67 (72 73 74) 88 89 94 (103 104 105) (110 111 112) 117 122 123 (134 135 136) 141 (146 147 148) 160 (169 170 171) (179 180 181))
(* 2 7 2 2 2 2 7 7 2 2 2 2 2 7 2 2 2 7 7 2 2 2 7 2 7 2 7 7)
