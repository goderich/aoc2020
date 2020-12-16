#lang racket

;; This day has a lot of moderately deeply nested structures, so the
;; threading library is a big help.
(require threading)

;; Part 1
;;
;; The input consists of three parts, separated by two newlines.
(define inputs
  (string-split (file->string "inputs/day16.txt") "\n\n"))

;; This helper function parses a line containing a ticket into a list of ints.
(define (line->field-values line)
  (~>> line
       (string-split _ ",")
       (map string->number)))

;; We use the above helper function to parse all nearby tickets.
(define nearby-tickets
  (~>> inputs
       (third)
       (string-split _ "\n")
       (rest)
       (map line->field-values)))

;; This function is for parsing numeric ranges in the first part of the input.
;; It returns a list of two lists, with two integers in each. That is the
;; range-pair that I'm using in the rest of the program.
(define (get-range-pair x)
  (define strs
    (~>> x
         (string-split)
         (take-right _ 3)))
  (define (get-range str)
    (map string->number
         (string-split str "-")))
  (define fst
    (get-range (first strs)))
  (define snd
    (get-range (third strs)))
  (list fst snd))

;; We use the above function to parse all range-pairs into a list.
(define ranges
  (~>> inputs
       (first)
       (string-split _ "\n")
       (map get-range-pair)))

;; This function determines if a given number N is in any of the two ranges
;; in a range-pair.
(define (in-range-pair? range-pair n)
  (define (in-single-range? r)
    (<= (first r) n (second r)))
  (ormap in-single-range? range-pair))

;; In part 1, a field value is considered possible if it fits in any of
;; the ranges in the input.
(define (possible-field-value? n)
  (for/or ((range-pair ranges))
    (in-range-pair? range-pair n)))

;; This function finds all the invalid values in a ticket and sums them up.
(define (invalid-value-sum ticket)
  (~>> ticket
       (filter-not possible-field-value?)
       (apply +)))

;; The answer to part 1 is the sum of all the invalid values in nearby tickets.
(for/sum ((ticket nearby-tickets))
  (invalid-value-sum ticket))

;; Part 2
;;
;; A ticket is valid iff all its values are within a possible range.
;; We find all the valid tickets and store them in a variable.
(define valid-tickets
  (filter (curry andmap possible-field-value?) nearby-tickets))

;; We also parse my ticket for use later.
(define my-ticket
  (~>> inputs
       (second)
       (string-split _ "\n")
       (second)
       (line->field-values)))

;; In order to find to correct positions of all the fields, we can use
;; the process of elimination. This function determines if a range-pair is
;; valid in a given position for all valid tickets.
(define (range-valid-in-pos? range-pair pos)
  (for/and ((ticket (cons my-ticket valid-tickets)))
    (in-range-pair? range-pair (list-ref ticket pos))))

;; We start by finding all possible positions for all fields.
(define candidates
  (for/list ((range-pair ranges))
    (for/list ((pos (in-range 0 20))
               #:when (range-valid-in-pos? range-pair pos))
      pos)))

;; This function is used to filter out the possibilities and hopefully
;; find the only possible positions for all fields.
(define (prune-candidates xs)
  (define (singleton-list? lst)
    (and (list? lst)
         (= 1 (length lst))))
  ;; We start by looking for lists containing only a single element,
  ;; extract it from the list, and delete it from all other lists.
  (define new-xs
    (for/first ((x xs)
                (i (in-naturals))
                #:when (singleton-list? x))
      (define singleton (car x))
      (define (remove-singleton item)
        (if (list? item)
            (remove singleton item)
            item))
      (define new-list
        (map remove-singleton xs))
      (list-set new-list i singleton)))
  ;; If there are still lists (i.e. undecided candidates), we call the
  ;; function again recursively, until the output is a list of numbers.
  (if (zero? (count list? xs))
      xs
      (prune-candidates new-xs)))

;; We use the above function to determine the correct position of each field.
(define field-positions
  (prune-candidates candidates))

;; The answer to part 2 is the product of all fields beginning with "departure"
;; in my ticket. These are the first six fields, so we can simply take the
;; first six positions in the field-positions list, find their values in the
;; ticket, and multiply them together.
(for/product ((index (take field-positions 6)))
  (list-ref my-ticket index))
