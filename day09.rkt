#lang racket

;; Part 1
;;
;; We begin by storing all the input numbers in a vector.
;; Vectors are similar to lists, but they have constant-time
;; access.
(define input
  (for/vector ((line (file->lines "inputs/day09.txt")))
    (string->number line)))

;; We write a predicate that determines if a number is valid
;; based on its index in the vector.
(define (valid? index)
  (define previous-25 (vector-copy input (- index 25) index))
  (for*/or ((x previous-25)
            (y previous-25)
            #:unless (= x y))
    (= (+ x y) (vector-ref input index))))

;; The answer to part 1 is the first number (starting at
;; index 25) that does NOT satisfy the validity predicate.
;; We store it in a variable because it's used in part 2.
(define first-invalid
  (for/first ((i (in-naturals 25))
              #:unless (valid? i))
    (vector-ref input i)))

;; Display the answer to part 1.
first-invalid

;; Part 2
;;
;; In this part we need to calculate sums of various slices
;; of the original input vector. We first write a helper
;; function that determines whether there is a contiguous
;; set satisfying the conditions of part 2 that begins at
;; index `start`. If there is one, it returns the set as a
;; vector. If not, it returns #f: it can thus be used as a
;; predicate.
(define (contiguous-set start)
  (define (loop acc end)
    (cond
      ((acc . = . first-invalid) (vector-copy input start end))
      ((acc . > . first-invalid) #f)
      (else (loop (+ acc (vector-ref input end))
                  (add1 end)))))
  (loop 0 start))

;; We then iterate over the list and find the first index
;; that begins a contiguous set, and calculate the sum of
;; its minimum and maximum values, as per the puzzle
;; requirements.
(for/first ((i (in-naturals))
            #:when (contiguous-set i))
  (define lst (vector->list (contiguous-set i)))
  (+ (apply min lst) (apply max lst)))
