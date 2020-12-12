#lang racket

(define instructions
  (file->lines "inputs/day12.txt"))

(struct coords (x y))
(struct vessel (dir coords))

(define (cardinal-direction-name ship)
  (match (vessel-dir ship)
    (0   "N")
    (90  "E")
    (180 "S")
    (270 "W")))

(define (move-cardinal pos dir num)
  (define x (coords-x pos))
  (define y (coords-y pos))
  (match dir
    ("N" (coords x (+ y num)))
    ("S" (coords x (- y num)))
    ("E" (coords (+ x num) y))
    ("W" (coords (- x num) y))))

(define (follow-part1-instruction ship instruction)
  (define dir (substring instruction 0 1))
  (define num (string->number (substring instruction 1)))
  (define pos (vessel-coords ship))
  (match dir
    ((or "N" "S" "E" "W") (vessel (vessel-dir ship) (move-cardinal pos dir num)))
    ("L" (vessel (modulo (+ (vessel-dir ship) (- 360 num)) 360) pos))
    ("R" (vessel (modulo (+ (vessel-dir ship) num) 360) pos))
    ("F" (follow-part1-instruction ship (format "~a~a" (cardinal-direction-name ship) num)))))


(define (manhattan-distance ship)
  (define x (coords-x (vessel-coords ship)))
  (define y (coords-y (vessel-coords ship)))
  (+ (abs x) (abs y)))

(for/fold ((ship (vessel 90 (coords 0 0)))
           #:result (manhattan-distance ship))
          ((instruction instructions))
  (follow-part1-instruction ship instruction))

(define (rotate-waypoint waypoint direction degrees)
  (define rotation (if (equal? direction "R")
                       degrees
                       (- 360 degrees)))
  (define x (coords-x waypoint))
  (define y (coords-y waypoint))
  (match rotation
    (90  (coords y (- x)))
    (180 (coords (- x) (- y)))
    (270 (coords (- y) x))))

(define (follow-part2-instruction ship waypoint instruction)
  (define dir (substring instruction 0 1))
  (define num (string->number (substring instruction 1)))
  (define pos-ship (vessel-coords ship))
  (match dir
    ((or "N" "S" "E" "W") (values ship (move-cardinal waypoint dir num)))
    ((or "L" "R") (values ship (rotate-waypoint waypoint dir num)))
    ("F" (values (vessel (vessel-dir ship)
                         (coords (+ (coords-x pos-ship) (* (coords-x waypoint) num))
                                 (+ (coords-y pos-ship) (* (coords-y waypoint) num))))
                 waypoint))))

(for/fold ((ship (vessel 90 (coords 0 0)))
           (waypoint (coords 10 1))
           #:result (manhattan-distance ship))
          ((instruction instructions))
  (follow-part2-instruction ship waypoint instruction))
