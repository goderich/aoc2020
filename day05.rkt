#lang racket

(require threading)

;; part 1
;;
;; Instead of calculating everything by hand,
;; the input strings can be directly translated
;; into zeros and ones and read as binary numbers.
;; This makes finding the seat IDs trivial.

(define (to-bit c)
  (match c
    ((or #\F #\L) #\0)
    ((or #\B #\R) #\1)))

(define (find-seat str)
  (~>> str
       (string->list)
       (map to-bit)
       (list->string)
       (string->number _ 2)))

;; We declare both seats and max-id as variables,
;; because we'll be using them in part 2

(define seats
  (for/list ((line (file->lines "inputs/day05.txt")))
    (find-seat line)))

(define max-id (apply max seats))

;; answer to part 1
max-id

;; part 2
;;
;; Since some seats are missing at the beginning of
;; the list, the highest seat number up to the max seat
;; ID (answer to part 1) not in the seats list will be
;; the answer. This is a good use case for for/last,
;; since the highest seat number satisfying the conditions
;; will turn up last in the iteration.

(for/last ((x (in-range max-id))
           #:when (not (member x seats)))
  x)
