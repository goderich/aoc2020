#lang racket/base

(require racket/file)

;; part 1

(define nums (map string->number (file->lines "inputs/day1.txt")))

(for*/first ((i nums)
             (j nums)
             #:when (= 2020 (+ i j)))
    (* i j))

;; part 2

(for*/first ((i nums)
             (j nums)
             (k nums)
             #:when (= 2020 (+ i j k)))
    (* i j k))
