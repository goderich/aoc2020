#lang racket

;; As usual, my solution includes liberal amounts of threading macros.
(require threading)

;; Part 1
;;
;; We first define a struct to hold the values of mem lines.
(struct mem (address value))

;; This is a helper function that parses a line like mem[x] = y, and
;; returns a mem struct.
(define (read-num line)
  (mem
   (~>> line
        (string-split)
        (first)
        (string->list)
        (filter char-numeric?)
        (list->string)
        (string->number))
   (~>> line
        (string-split)
        (third)
        (string->number))))

;; This helper function pads strings with zeros in the front, up to the
;; length of 36, which is defined in the puzzle.
(define (pad numstr)
    (define zeros (make-string (- 36 (string-length numstr)) #\0))
    (format "~a~a" zeros numstr))

;; This function applies a mask to a decimal number and returns the
;; resulting decimal number.
(define (apply-mask num mask)
  (define (masked-charlist numstr)
    (for/list ((n (in-string numstr))
               (m (in-string mask)))
      (if (eq? m #\X) n m)))
  (~>> num
       (number->string _ 2)
       (pad)
       (masked-charlist)
       (list->string)
       (string->number _ 2)))

;; To get the answer to part 1, we iterate through the input, storing the
;; current masks, applying them to mem rows, and putting the resulting
;; numbers into a hash map. The answer is the sum of all values in the hash
;; map.
;;
;; I use `cond` instead of `if` in the body, because the former allows me
;; to use several expressions after a single condition, so I can put defines
;; inside the body of the cond.
(for/fold ((dict (make-immutable-hasheq))
           (mask "")
           #:result (apply + (hash-values dict)))
          ((line (file->lines "inputs/day14.txt")))
  (cond
   ((string-prefix? line "mask")
    (values dict (third (string-split line))))
   (else
    (define memcell (read-num line))
    (define key (mem-address memcell))
    (define val (apply-mask (mem-value memcell) mask))
    (values (hash-set dict key val) mask))))

;; Part 2
;;
;; This function generates all memory addresses when a mask is applied
;; to a memory address. The result is a list of decimal numbers.
;;
;; I use the compose1 function in the result clause to string several
;; functions together, somewhat similar to threading macros, except the
;; functions on the right are applied first. Unlike `compose`, `compose1`
;; checks that only a single value is passed through all functions.
(define (decode-memory-addresses num mask)
  (define numstr (pad (number->string num 2)))
  (define (from-binstr str)
    (string->number str 2))
  (for/fold ((acc '(()))
             #:result (map (compose1 from-binstr list->string reverse) acc))
            ((n (in-string numstr))
             (m (in-string mask)))
    (match m
      (#\0 (map (curry cons n) acc))
      (#\1 (map (curry cons #\1) acc))
      (#\X (append (map (curry cons #\0) acc)
                   (map (curry cons #\1) acc))))))

;; To get the answer to part 2, we use a similar fold to part 1, but this
;; time with the `decode-memory-addresses` function providing the list of
;; memory addresses.
(for/fold ((dict (make-immutable-hasheq))
           (mask "")
           #:result (apply + (hash-values dict)))
          ((line (file->lines "inputs/day14.txt")))
  (cond
    ((string-prefix? line "mask")
     (values dict (third (string-split line))))
    (else
     (define memcell (read-num line))
     (define keys (decode-memory-addresses (mem-address memcell) mask))
     (define val (mem-value memcell))
     (define new-dict
       (for/fold ((d dict))
                 ((key keys))
         (hash-set d key val)))
     (values new-dict mask))))
