#lang racket

(require threading)

;; We begin by reading all the input numbers into a list.
(define input
  (~>>
   (file->string "inputs/day15.txt")
   (string-trim)
   (string-split _ ",")
   (map string->number)))

;; We make a hash map containing all but one of the numbers in the input as keys,
;; with their latest indices as values. This serves as the initial value of the dict
;; variable in the main loop below.
(define dict-init
  (for/hasheq ((k (drop-right input 1))
               (v (in-naturals 1)))
    (values k v)))

;; The list of numbers is iterated within this function.
;; At each step, we check if the newest number already exists in the dict, and
;; calculate the next number according to the rules.
;; We start by running the last item of the input through the rules.
;; The loop terminates according to a given value N, which will output the Nth
;; number of the sequence.
(define (nth-number n)
  (for/fold ((dict dict-init)
             (newest (last input))
             #:result newest)
            ((i (in-naturals (length input)))
             #:break (= i n))
    (values (hash-set dict newest i)
            (cond
              ((hash-ref-key dict newest #f) (- i (hash-ref dict newest)))
              (else 0)))))

;; Answer to part 1.
(nth-number 2020)

;; Answer to part 2.
;; This runs for 30 seconds, which is a bit long, but acceptable enough that
;; I don't feel any immediate need to optimize it (if it's even possible).
(nth-number 30000000)
