#lang racket/base

(require racket/file)

;; We begin by reading all the numbers into a list of ints.
(define nums (map string->number (file->lines "inputs/day01.txt")))

;; Racket's looping tools are an excellent match for this task. Here we
;; have a nested loop where we are looking for the first occurrence of
;; variables that satisfy a certain condition: a perfect match for the
;; `for*/first` loop with the `#:when` keyword.
(for*/first ((i nums)
             (j nums)
             #:when (= 2020 (+ i j)))
    (* i j))

;; For part 2 we just add a third variable to the loop, condition, and
;; answer.
(for*/first ((i nums)
             (j nums)
             (k nums)
             #:when (= 2020 (+ i j k)))
    (* i j k))
