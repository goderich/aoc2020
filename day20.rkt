#lang racket

;; WARNING !!! SPAGHETTI BELOW !!!
;; This code should not be read for fear of blindness and/or insanity.
;; I'm provisionally uploading it to GitHub, but I will be back to retouch it a bit.
(require threading)

;; Part 1
;;
;; This helper function reads a tile, and parses into into a cons cell of
;; the tile number and the tile chars as a list of lists, to be stored in
;; a hash map.
(define (read-tile str)
  (define rows (string-split str "\n"))
  (define tilenum
    (~>> rows
         (first)
         (string->list)
         (filter char-numeric?)
         (list->string)
         (string->number)))
  (define tilechars
    (~>> rows
         (rest)
         (map string->list)))
  (cons tilenum tilechars))

;; We read the input and construct a mutable hash map with tile nums as
;; the keys, as tile grids (lists of lists of chars) as their values. We
;; will be mutating the values later.
(define tiles
  (~>>
   (file->string "inputs/day20.txt")
   (string-split _ "\n\n")
   (map read-tile)
   (make-hasheq)))

;; Return the borders of a tile in the order: top, right, bottom, left.
(define (borders tile)
  (list
   (first tile)
   (map last tile)
   (last tile)
   (map first tile)))

;; Rotate a matrix (list of lists) clockwise 90 degrees.
(define (rotate matrix)
  (for/list ((col (in-range (length (car matrix)))))
    (for/list ((row (reverse matrix)))
      (list-ref row col))))

;; Return all 8 possible rotations and flips of a tile.
(define (rotations tile)
  (define mirror-fs (list identity reverse))
  (define rotate-fs (list identity
                          rotate
                          ;; The following is faster than composing
                          ;; multiple instances of `rotate`:
                          (compose reverse (curry map reverse))
                          (compose rotate reverse (curry map reverse))))
  (for*/list ((mirror-f mirror-fs)
              (rotate-f rotate-fs))
    (mirror-f (rotate-f tile))))

;; We make another mutable hash map, this time storing coordinates as keys
;; and tile numbers as values. We will use it to store the relative coordinates
;; of all tiles, as determined by the `find-neighbours!` function.
(define coords (make-hash))

;; I don't like the way this function is written, but it simplifies things
;; in `find-neighbours!` if we treat top, right, bottom, left as integers.
;; This function relies on the aforementioned assumption, and calculates
;; the coordinates of a point's neighbour given the direction int.
(define (find-point point i)
  (match-define (cons x y) point)
  (match i
    (0 (cons x (add1 y)))
    (1 (cons (add1 x) y))
    (2 (cons x (sub1 y)))
    (3 (cons (sub1 x) y))))

;; It took me several hours to finally come to terms with the fact that
;; I couldn't find a solution that would be both purely functional and also
;; understandable. It really is a lot simpler to use mutable hash maps for
;; this problem. The good news is, we only need to mutate variables in this
;; function, and so, like the good Racketeers we are, we put an exclamation
;; mark at the end of its name to let people know it has side effects.
;;
;; We start at a given POINT, our input.
(define (find-neighbours! point)
  (define tilenum (hash-ref coords point))
  ;; Then we iterate over the borders of the tile on that point.
  (for ((border (borders (hash-ref tiles tilenum)))
        ;; We also count the sequential number of the border, to be used below.
        (i (in-naturals)))
    (for/first (((n t) tiles)
                ;; We iterate over all tiles, skipping the ones we've already
                ;; added to the coords hash map.
                #:unless (member n (hash-values coords))
                ;; For the rest, we check all borders and their reverse, to see
                ;; if they match our current border. This way we don't have to
                ;; rotate anything until we find a matching tile.
                #:when (member border (append (borders t) (map reverse (borders t))))
                ;; Only then do we generate all rotations, and look for the one
                ;; whose matching border is on the opposite side of our current
                ;; border index.
                (rot (rotations t))
                #:when (equal? border (list-ref (borders rot) (remainder (+ i 2) 4))))
      ;; Once found, that rotation replaces the original in the main tiles hash map.
      (hash-set! tiles n rot)
      ;; After which we add the coordinates of the newly found neighbour to the
      ;; coords map, and looks for its neighbours recursively.
      (define neighbour-coords (find-point point i))
      (hash-set! coords neighbour-coords n)
      (find-neighbours! neighbour-coords))))

;; We start the loop by taking the first tile and assigning it the coordinates
;; x = 0 and y = 0. The remainder of the grid is built relative to this first
;; tile. The good thing about this is we don't have to waste time and effort
;; looking for a starting point. The drawback is that we have no idea what the
;; actual range of coordinates will be, and have to waste time and effort
;; finding them (though only a little bit in this case).
(hash-set! coords '(0 . 0) (car (hash-keys tiles)))
(find-neighbours! '(0 . 0))

;; Once the coords have all been found, we examine the values of the keys to
;; find the highest and lowest values of x and y, and through these, the corners.
(define xs (map car (hash-keys coords)))
(define ys (map cdr (hash-keys coords)))
(define min-x (apply min xs))
(define max-x (apply max xs))
(define min-y (apply min ys))
(define max-y (apply max ys))
(define corner-coords (list (cons min-x min-y)
                            (cons max-x min-y)
                            (cons min-x max-y)
                            (cons max-x max-y)))

;; We multiply the numbers of the corner tiles to get the answer to part 1.
(apply * (map (curry hash-ref coords) corner-coords))

;; Part 2

(define (remove-borders tile)
  (~>> tile
       (rest)
       (drop-right _ 1)
       (map rest)
       (map (λ (row) (drop-right row 1)))))

(define ordered-tiles
  (for*/list ((y (in-range max-y (sub1 min-y) -1))
              (x (in-range min-x (add1 max-x))))
    (remove-borders (hash-ref tiles (hash-ref coords (cons x y))))))

(define picture
  (for*/list ((row (in-slice 12 ordered-tiles))
              (y (in-range (length (car ordered-tiles)))))
    (apply append (map (λ (lst) (list-ref lst y)) row))))

(define sea-monster
  (~>>
   '("..................#."
     "#....##....##....###"
     ".#..#..#..#..#..#...")
   (map string->list)
   (map (λ (lst) (indexes-of lst #\#)))))

(define (check-for-monsters grid)
  (for*/fold ((acc 0))
             ((row (in-range ((length grid) . - . 3)))
              (col (in-range ((length (car grid)) . - . 20))))
    (define monster?
      (for*/and ((linenum (in-range 3))
                 (pos (list-ref sea-monster linenum)))
        (eq? #\# (list-ref (list-ref grid (+ row linenum)) (+ col pos)))))
    (if monster?
        (add1 acc)
        acc)))

(for/first ((rot (rotations picture))
            #:when (positive? (check-for-monsters rot)))
  (define length-of-monster
    (length (flatten sea-monster)))
  (- (count (curry eq? #\#) (flatten rot))
     (* length-of-monster (check-for-monsters rot))))
