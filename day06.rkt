#lang racket

;; We first split the input into groups, to be used for
;; both part 1 and part 2.
(define groups
  (string-split (file->string "inputs/day06.txt") "\n\n"))

;; part 1
;;
;; This is a very straightforward count of all the letters
;; that appear in a group at least once.
(for/sum ((group groups))
  (length
   (remove-duplicates
    (string->list
     (string-replace group "\n" "")))))

;; part 2
;;
;; Here we will use intersections of sets to determine
;; which letters appear on every line of a group. Due to
;; the way set-intersect works in Racket, the first set
;; has to be the complete alphabet. Since we're using it
;; on every iteration, we might as well declare it ouside
;; the fold.
(define alphabet-list
  (string->list "abcdefghijklmnopqrstuvwxyz"))

;; We use two nested folds. The inner fold counts the
;; number of chars that occur on every line of a group.
;; The outer fold sums up the results to give us the answer.
;; Note that the set-intersect function can be used on
;; lists directly.
(for/sum ((group groups))
  (for/fold ((acc alphabet-list)
             #:result (length acc))
            ((line (string-split group)))
    (set-intersect acc (string->list line))))
