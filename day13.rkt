#lang racket

;; Today's input is just two lines.
(define input (file->lines "inputs/day13.txt"))
;; The first line is the starting time for part 1 of the puzzle.
(define mintime (string->number (first input)))

;; Before we read in the buses, we first declare a struct containing the
;; bus ID and its sequential number in the input, which serves as the offset
;; for part 2 of the puzzle.
(struct bus (num offset))

;; This is a helper function to take a list of bus IDs and convert them into
;; bus structs. We discard buses without numbers in this function as well.
(define (read-buses lst)
  (for/list ((i (in-naturals))
             (element lst)
             #:when (not (equal? element "x")))
    (bus (string->number element) i)))

;; We use the above to read the second line into a list of bus structs.
(define buses (read-buses (string-split (second input) ",")))

;; Part 1
;;
;; This function finds the first bus that has a departure in a given minute.
;; Because of the way for/first works, if no elements satisfy the #:when
;; clause, it returns #f, so it can double as a predicate.
(define (departure? minute)
  (for/first ((b buses)
              #:when (zero? (remainder minute (bus-num b))))
    (bus-num b)))

;; We use the above function to find the answer to part 1.
(for/first ((minute (in-naturals mintime))
            #:when (departure? minute))
  (* (departure? minute) (- minute mintime)))

;; Part 2
;;
;; This function finds the lowest number that satisfies the conditions of
;; part 2 of the puzzle, but only for two buses (here I just use the words
;; divisor and offset). The bus numbers are all primes, so after this first
;; number the next ones will come every divisor1 * divisor2 increments.
(define (lowest-offset divisor1 offset1 divisor2 offset2)
  (for/first ((i (in-range offset1 +inf.0 divisor1))
              #:when (zero? (remainder (+ i offset2) divisor2)))
    i))

;; To find the answer to part 2, we will fold through the list of buses, each
;; time finding the new offset with the lowest-offset function, and multiplying
;; the bus number with the current step to get the new step (increments after
;; which the pattern repeats again). The step serves as divisor1 in the
;; lowest-offset function, guaranteeing that the conditions are satisfied for
;; all previous buses. When all buses have been iterated through, the final
;; offset value will be the answer: the lowest number that satisfies the
;; conditions for all buses.
(for/fold ((step 1)
           (starting-point 1)
           #:result starting-point)
          ((b buses))
  (values (* step (bus-num b))
          (lowest-offset step starting-point (bus-num b) (bus-offset b))))
