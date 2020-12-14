#lang racket

;; NB. I later found out that some very smart people used complex numbers
;; to solve this problem. That's very impressive, however I decided to
;; keep my original solution. It's not as clever, but it's mine.

;; Part 1
;;
;; We begin by reading in the instructions and storing them both use
;; in both parts of the puzzle.
(define instructions
  (file->lines "inputs/day12.txt"))

;; We declare two structs to be used later on: coordinates (both absolute and
;; relative coordinates), and a vessel struct, which has coordinates and a
;; direction and is used for ships only. Direction is represented as an
;; integer, in degrees.
(struct coords (x y))
(struct vessel (dir coords))

;; This is a helper function to convert a direction in degrees to its string
;; representation, used in move forward commands in part 1.
(define (cardinal-direction-name ship)
  (match (vessel-dir ship)
    (0   "N")
    (90  "E")
    (180 "S")
    (270 "W")))

;; This helper function moves a set of coordinates NUM units in direction DIR.
(define (move-cardinal pos dir num)
  (define x (coords-x pos))
  (define y (coords-y pos))
  (match dir
    ("N" (coords x (+ y num)))
    ("S" (coords x (- y num)))
    ("E" (coords (+ x num) y))
    ("W" (coords (- x num) y))))

;; This function executes one instruction using part 1 rules.
;; Note that "F" is given back to the function itself as a NSEW
;; movement instruction.
(define (follow-part1-instruction ship instruction)
  (define dir (substring instruction 0 1))
  (define num (string->number (substring instruction 1)))
  (define pos (vessel-coords ship))
  (match dir
    ((or "N" "S" "E" "W") (vessel (vessel-dir ship) (move-cardinal pos dir num)))
    ("L" (vessel (modulo (+ (vessel-dir ship) (- 360 num)) 360) pos))
    ("R" (vessel (modulo (+ (vessel-dir ship) num) 360) pos))
    ("F" (follow-part1-instruction ship (format "~a~a" (cardinal-direction-name ship) num)))))

;; Returns the Manhattan distance from (0 0).
(define (manhattan-distance ship)
  (define x (coords-x (vessel-coords ship)))
  (define y (coords-y (vessel-coords ship)))
  (+ (abs x) (abs y)))

;; The answer to part 1.
(for/fold ((ship (vessel 90 (coords 0 0)))
           #:result (manhattan-distance ship))
          ((instruction instructions))
  (follow-part1-instruction ship instruction))

;; Part 2
;;
;; This function rotates the waypoint. Left turns are converted into right
;; turns, and there are only turns of 90, 180, and 270 degrees in the input.
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

;; This function executes one instruction using part 2 rules.
;; Since some instructions move the ship and others the waypoint,
;; it returns both the ship and the waypoint.
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

;; The answer to part 2.
;; The waypoint is stored as a coords struct, since it has no direction
;; property. Its coordinates are relative to the ship, but this does not
;; impact the calculations in any way.
(for/fold ((ship (vessel 90 (coords 0 0)))
           (waypoint (coords 10 1))
           #:result (manhattan-distance ship))
          ((instruction instructions))
  (follow-part2-instruction ship waypoint instruction))
