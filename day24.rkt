#lang racket

;; The Megaparsack library is extremely useful for today's problem.
(require megaparsack
         megaparsack/text
         data/either)

;; For ease of comprehension, we declare a coordinates struct with
;; two integer values. We make it `#:transparent` for easier access
;; without getters (e.g. via `match-define`, as used below).
(struct coords (x y) #:transparent)

;; Since today's puzzle uses a hex grid, we space out each point
;; by a Manhattan distance of 2: either 2 in a straight line
;; (when going east or west), or 1 vertical and 1 horizontal
;; (when moving diagonally).
(define (move point direction)
  (match-define (coords x y) point)
  (match direction
    ("e" (coords (+ x 2) y))
    ("se" (coords (add1 x) (sub1 y)))
    ("sw" (coords (sub1 x) (sub1 y)))
    ("w" (coords (- x 2) y))
    ("nw" (coords (sub1 x) (add1 y)))
    ("ne" (coords (add1 x) (add1 y)))))

;; A line is just a sequence of commands, out of a possible 6. We
;; use megaparsack to parse each line into a list (using many/p).
(define direction/p
  (many/p
   ;; For strings starting with "s" and "n", we need to use the
   ;; try/p mechanism to make sure we don't get stuck.
   (or/p (try/p (string/p "se"))
         (string/p "sw")
         (try/p (string/p "nw"))
         (string/p "ne")
         (string/p "w")
         (string/p "e"))))

;; We parse each line of instructions and calculate its final point,
;; collecting them all into a list.
(define points
  (for/list ((line (file->lines "inputs/day24.txt")))
    (for/fold ((point (coords 0 0)))
              ((direction (from-success #f (parse-string direction/p line))))
      (move point direction))))

;; We then iterate over the list of final points, collecting the results
;; into a hash set representing the coordinates of black tiles (this is also done
;; with part 2 in mind). If a point already exists in the set, we remove
;; it from the set, thus imitating a flip from black to white.
(define black-tiles
  (for/fold ((st (set)))
            ((point points))
    (if (set-member? st point)
        (set-remove st point)
        (set-add st point))))

;; The answer to part 1 is the number of black tiles.
(set-count black-tiles)

;; Part 2
;;
;; This is yet another version of the Game of Life, of which there have
;; already been multiple variations during this year's AoC. We will use
;; a set of black tile coordinates, and check the adjacent tiles of each
;; tile every turn. We don't need to include the tiles from the set
;; themselves, just their neighbours: if a tile has neighbours it will
;; still appear in the set, and if it doesn't have neighbours it gets
;; flipped to white (i.e. removed from the set), so we can ignore it.
;;
;; We begin by writing a few helper functions. This function returns a
;; list of all tiles adjacent to POINT.
(define (adjacent point)
  (match-define (coords x y) point)
  (list
    (coords (+ x 2) y)
    (coords (add1 x) (sub1 y))
    (coords (sub1 x) (sub1 y))
    (coords (- x 2) y)
    (coords (sub1 x) (add1 y))
    (coords (add1 x) (add1 y))))

;; This function calculates how many of the tiles in ST adjacent to POINT
;; are black.
(define (adjacent-black st point)
  (count (curry set-member? st) (adjacent point)))

;; This function checks the conditions for a tile to flip or remain its
;; previous colour. More specifically, it returns #t if the tile will be
;; black the next day.
(define (stays-black? st tile)
  (cond
    ;; black -> white
    ((and (set-member? st tile)
          (or (zero? (adjacent-black st tile))
              ((adjacent-black st tile) . > . 2))) #f)
    ;; white -> black
    ((and (not (set-member? st tile))
          (= 2 (adjacent-black st tile))) #t)
    ;; black -> black
    ((set-member? st tile) #t)
    ;; white -> white
    (else #f)))

;; To get the answer to part 2, we run the Game of Life simulation for
;; 100 days, and then count the black tiles.
(for/fold ((st black-tiles)
           #:result (set-count st))
          ((_ 100))
  ;; We iterate over the list of all neighbours of all tiles, deduplicated.
  (for/set ((tile (remove-duplicates (flatten (set-map st adjacent))))
            #:when (stays-black? st tile))
    tile))
