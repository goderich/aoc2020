#lang racket

;; I'm using the threading library today again.
(require threading)

;; Part 1
;;
;; We read in the input and store it as a vector of vectors, a sort of
;; matrix, for easy access to individual positions.
(define input
  (~>> "inputs/day11.txt"
       (file->lines)
       (map string->list)
       (map list->vector)
       (list->vector)))

;; The width and height of the grid never change, so they can be calculated
;; once and referred to many times later.
(define grid-width
  (vector-length (vector-ref input 0)))

(define grid-height
  (vector-length input))

;; We define a struct for positions, where y is the row number, and x is the
;; seat starting from the left (the normal x and y of a Cartesian grid). Both
;; x and y are zero-indexed.
(struct coords (x y))

;; Since the width and height of the grid are constant, we can check if a point
;; is outside the grid using only its coordinates.
(define (out-of-grid? point)
  (define x (coords-x point))
  (define y (coords-y point))
  (or (x . < . 0)
      (y . < . 0)
      (x . >= . grid-width)
      (y . >= . grid-height)))

;; This function returns the current values of all adjacent seats of a given
;; point as a list.
(define (adjacent-seats point grid)
  (define x (coords-x point))
  (define y (coords-y point))
  (define (to-coords lst)
    (coords (first lst) (second lst)))
  (define neighbours
    (map to-coords
         (remove (list x y)
                 (cartesian-product (list (sub1 x) x (add1 x))
                                    (list (sub1 y) y (add1 y))))))
  (map (λ (p) (seat-status p grid))
       (filter-not out-of-grid? neighbours)))

;; This is a helper function to find the current status of a given point
;; on a grid.
(define (seat-status point grid)
  (vector-ref (vector-ref grid (coords-y point)) (coords-x point)))

;; This is a helper function to check how many of the seats in a list
;; are occupied.
(define (num-occupied seats)
  (length (filter (curry eq? #\#) seats)))

;; This is a function that changes the status of a grid after one turn,
;; abstracted to be used with both part 1 and part 2. Since the only
;; difference between the two parts is the function used to check for
;; neighbours, and the maximum amount of neighbours tolerated, these
;; are taken as variables, to be passed by wrapper functions.
(define (step-abstract grid neighbouring-f max-neighbours)
  (for/vector ((y (in-range grid-height)))
    (for/vector ((x (in-range grid-width)))
      (define adjacent (neighbouring-f (coords x y) grid))
      (define current-status (seat-status (coords x y) grid))
      (cond
        ((eq? #\. current-status) #\.)
        ((zero? (num-occupied adjacent)) #\#)
        (((num-occupied adjacent) . >= . max-neighbours) #\L)
        (else current-status)))))

;; This is a wrapper function around `step-abstract` for part 1.
(define (step grid)
  (step-abstract grid adjacent-seats 4))

;; This function returns the total amount of occupied seats on a grid.
(define (occupied grid)
  (~>> grid
       (vector->list)
       (map vector->list)
       (flatten)
       (count (curry eq? #\#))))

;; The main engine of this solution. This function runs recursively,
;; calling the appropriate step-abstract wrapper on a grid until its
;; state stabilizes, after which it returns the answer.
(define (game-of-life step-f grid)
  (define new-grid (step-f grid))
  (if (equal? grid new-grid)
      (occupied grid)
      (game-of-life step-f new-grid)))

;; Return the answer to part 1.
(game-of-life step input)

;; Part 2
;;
;; This function returns the coordinates after moving STEPS number of steps
;; in direction DIR from POINT.
(define (move-in-dir point steps dir)
  (define x (coords-x point))
  (define y (coords-y point))
  (match-define (list new-x new-y) (map + (map (curry * steps) dir) (list x y)))
  (coords new-x new-y))

;; Directions are constant, represented as two numbers from -1 to 1.
(define directions
  (remove '(0 0) (cartesian-product '(-1 0 1) '(-1 0 1))))

;; This function finds the first seat in every direction, returning a list.
;; If no seat is found in a given direction, it does not appear in the list,
;; getting deleted by the `filter identity` function (which removes #f).
(define (first-in-direction point grid)
  (define firsts
    (for/list ((dir directions))
      (define (loop i)
        (define new-point (move-in-dir point i dir))
        (define (not-floor? c)
          (if (eq? #\. c) #f c))
        (cond
          ((out-of-grid? new-point) #f)
          ;; Originally I had this line instead of the one below:
          ;; ((not (eq? #\. (seat-status new-point grid))) (seat-status new-point grid))
          ;; But I disliked that `seat-status` was called twice, so I wrote the helper
          ;; function `not-floor?` which returns #f on a floor tile, and the
          ;; value of the seat if it's a seat. Thus it returns a truthy value
          ;; on a seat, which is then passed to the right-hand side using =>,
          ;; and returned as-is using the `identity` function.
          ((not-floor? (seat-status new-point grid)) => identity)
          (else (loop (add1 i)))))
      (loop 1)))
  (filter identity firsts))

;; This is a wrapper function around `step-abstract` for part 1.
(define (step2 grid)
  (step-abstract grid first-in-direction 5))

;; Return the answer to part 2.
(game-of-life step2 input)
