#lang racket

;; We begin by defining a parameter of dimensions used in the game of life.
;; This parameter is limited by contract to only values of 3 or 4. Its
;; default value is 3, as used in part 1.
(define/contract dimensions
  (parameter/c (or/c 3 4))
  (make-parameter 3))

;; We use a nullary function to read the input file. The function examines
;; the `dimensions` parameter to decide whether to output lists of length
;; 3 or 4 for the coordinates of individual cubes. The coordinates of
;; currently occupied positions are collected into a set. We begin the game
;; with z (and w, if applicable) set to 0, but this isn't important: it
;; can be any number.
(define (get-input)
  (for/set ((line (file->lines "inputs/day17.txt"))
            (y (in-naturals))
            #:when #t ; so that the clauses below are nested
            (char (in-string line))
            (x (in-naturals))
            #:when (eq? #\# char))
    (cond
      ((= 3 (dimensions)) (list x y 0))
      ((= 4 (dimensions)) (list x y 0 0)))))

;; This function returns the coordinates of all cubes that are touching
;; a given cube. It works on both 3- and 4-dimensional cubes.
(define (adjacent-cubes cube)
  (define (plus-minus-one x)
    (list (sub1 x) x (add1 x)))
  (remove cube
          (apply cartesian-product
                 (map plus-minus-one cube))))

;; This function returns the state of the grid after a single cycle.
(define (step grid)
  (define (occupied-neighbours cube)
    (count (curry set-member? grid) (adjacent-cubes cube)))
  ;; Because `adjacent-cubes` does not return `coord` itself, we are
  ;; not checking it in the inner loop, but this is fine: if it has
  ;; any occupied neighbours, it will be checked when those neighbours
  ;; are the `coord`, and if it doesn't, we won't be including it in
  ;; the next grid anyway.
  (for*/set ((coord grid)
             (cube (adjacent-cubes coord))
             #:when (or (and (not (set-member? grid cube))
                             (= 3 (occupied-neighbours cube)))
                        (and (set-member? grid cube)
                             (<= 2 (occupied-neighbours cube) 3))))
    cube))

;; The task of the puzzle is to return the amount of active cubes
;; after 6 cycles.
(define (run-cycles)
  (for/fold ((grid (get-input))
             #:result (set-count grid))
            ((i (in-naturals))
             #:break (= i 6))
    (step grid)))

;; To get the answer to part 1, we run the function as-is.
(run-cycles)

;; For part 2, we parameterize the function with 4 dimensions.
(parameterize ((dimensions 4))
  (run-cycles))
