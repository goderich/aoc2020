#lang racket

(require threading)

(define input
  (~>> "inputs/day11.txt"
       (file->lines)
       (map string->list)
       (map list->vector)
       (list->vector)))

(define grid-width
  (vector-length (vector-ref input 0)))

(define grid-height
  (vector-length input))

(struct coords (x y))

(define (out-of-grid? point)
  (define x (coords-x point))
  (define y (coords-y point))
  (or (x . < . 0)
      (y . < . 0)
      (x . >= . grid-width)
      (y . >= . grid-height)))

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

(define (seat-status point grid)
  (vector-ref (vector-ref grid (coords-y point)) (coords-x point)))

(define (none-occupied? seats)
  (andmap (curry (negate eq?) #\#) seats))

;; This function takes a list
(define (num-occupied seats)
  (length (filter (curry eq? #\#) seats)))

(define (step-abstract grid neighbouring-f max-neighbours)
  (for/vector ((y (in-range grid-height)))
    (for/vector ((x (in-range grid-width)))
      (define adjacent (neighbouring-f (coords x y) grid))
      (define current-status (seat-status (coords x y) grid))
      (cond
        ((eq? #\. current-status) #\.)
        ((none-occupied? adjacent) #\#)
        (((num-occupied adjacent) . >= . max-neighbours) #\L)
        (else current-status)))))

(define (step grid)
  (step-abstract grid adjacent-seats 4))

(define (occupied grid)
  (~>> grid
       (vector->list)
       (map vector->list)
       (flatten)
       (count (curry eq? #\#))))

(define (game-of-life step-f grid)
  (define new-grid (step-f grid))
  (if (equal? grid new-grid)
      (occupied grid)
      (game-of-life step-f new-grid)))

(game-of-life step input)

;; part 2

(define (move-in-dir point steps dir)
  (define x (coords-x point))
  (define y (coords-y point))
  (match-define (list new-x new-y) (map + (map (curry * steps) dir) (list x y)))
  (coords new-x new-y))

(define directions
  (remove '(0 0)
          (cartesian-product '(-1 0 1) '(-1 0 1))))

(define (first-in-direction point grid)
  (define firsts
    (for/list ((dir directions))
      (define (loop i)
        (define new-point (move-in-dir point i dir))
        (define (not-floor? c)
          (if (eq? #\. c) #f c))
        (cond
          ((out-of-grid? new-point) #f)
          ;; ((not (eq? #\. (seat-status new-point grid))) (seat-status new-point grid))
          ((not-floor? (seat-status new-point grid)) => identity)
          (else (loop (add1 i)))))
      (loop 1)))
  (filter identity firsts))

(define (step2 grid)
  (step-abstract grid first-in-direction 5))

(game-of-life step2 input)
