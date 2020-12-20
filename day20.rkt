#lang racket

;; WARNING !!! SPAGHETTI BELOW !!!
;; This code should not be read for fear of blindness and/or insanity.
;; I'm provisionally uploading it to GitHub, but I will be back to retouch it a bit.
(require threading)

(define tiles
  (~>>
   (file->string "inputs/day20.txt")
   (string-split _ "\n\n")
   (map (λ (s) (cons (string->number (list->string (filter char-numeric? (string->list (first (string-split s "\n"))))))
                     (map string->list (rest (string-split s "\n"))))))
   (make-hasheq)))

(define (borders tile)
  (list
   (first tile)
   (map last tile)
   (last tile)
   (map first tile)))

;; rotate clockwise 90 degrees
(define (rotate matrix)
  (for/list ((col (in-range (length (car matrix)))))
    (for/list ((row (reverse matrix)))
      (list-ref row col))))

(define (rotations tile)
  (define mirror-fs (list identity reverse))
  (define rotate-fs (list identity
                          rotate
                          (compose rotate rotate)
                          (compose rotate rotate rotate)))
  (for*/list ((mirror-f mirror-fs)
              (rotate-f rotate-fs))
    (mirror-f (rotate-f tile))))

(define coords (make-hash (list (cons (car (hash-keys tiles)) '(0 . 0) ))))

(define (find-point point i)
  (match-define (cons x y) point)
  (match i
    (0 (cons x (sub1 y)))
    (1 (cons (add1 x) y))
    (2 (cons x (add1 y)))
    (3 (cons (sub1 x) y))))

(define (find-neighbours! tilenum)
  (for ((border (borders (hash-ref tiles tilenum)))
        (i (in-naturals)))
    (for/first (((n v) tiles)
          #:unless (hash-has-key? coords n)
          #:when (member border (append (borders v) (map reverse (borders v))))
          (t (rotations v))
          #:when (equal? border (list-ref (borders t) (remainder (+ i 2) 4))))
      (hash-set! tiles n t)
      (hash-set! coords n (find-point (hash-ref coords tilenum) i))
      (find-neighbours! n))))

(find-neighbours! (car (hash-keys tiles)))

(define xs (map car (hash-values coords)))
(define ys (map cdr (hash-values coords)))
(define min-x (apply min xs))
(define max-x (apply max xs))
(define min-y (apply min ys))
(define max-y (apply max ys))
(define corner-coords (list (cons min-x min-y)
                            (cons max-x min-y)
                            (cons min-x max-y)
                            (cons max-x max-y)))
(define d
  (for/hash (((k v) coords))
    (values v k)))

(apply * (map (curry hash-ref d) corner-coords))

;; part 2

(define (remove-borders tile)
  (~>> tile
       (rest)
       (drop-right _ 1)
       (map rest)
       (map (λ (row) (drop-right row 1)))))

(define l
  (for*/list ((y (in-range min-y (add1 max-y)))
              (x (in-range min-x (add1 max-x))))
    (remove-borders (hash-ref tiles (hash-ref d (cons x y))))))

(define picture
  (for*/list ((row (in-slice 12 l))
              (y (in-range (length (car l)))))
    (apply append (map (λ (lst) (list-ref lst y)) row))))

(define sea-monster
  (~>>
   (map string->list
        '("..................#."
          "#....##....##....###"
          ".#..#..#..#..#..#..."))
   (map (λ (lst) (indexes-of lst #\#)))))

(define (check-for-monster lstlst)
  (count identity
         (for*/list ((row (in-range ((length lstlst) . - . 3)))
                     (col (in-range ((length (car lstlst)) . - . 20))))
           (for/and ((line sea-monster)
                     (linenum (in-naturals))
                     #:when #t
                     (pos line))
             (eq? #\# (list-ref (list-ref lstlst (+ row linenum)) (+ col pos)))))))

(define length-of-monster
  (length (flatten sea-monster)))

(for/first ((pic (rotations picture))
            #:when (positive? (check-for-monster pic)))
  (- (count (curry eq? #\#) (flatten pic))
     (* length-of-monster (check-for-monster pic))))
