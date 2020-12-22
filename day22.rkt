#lang racket

(require threading)

;; We begin, as usual, by reading and parsing the input, this time into
;; two lists of integers.
(define decks
  (~>>
   (file->string "inputs/day22.txt")
   (string-split _ "\n\n")
   (map (λ (s) (string-split s "\n")))
   (map rest)
   (map (curry map string->number))))

;; Part 1 is very straightforward. We check if any of the lists are empty,
;; and if they are, proceed to calculate the winner's score, otherwise
;; check the round's winner.
(define (combat d1 d2)
  (cond
    ((null? d1) (score d2))
    ((null? d2) (score d1))
    (((car d1) . < . (car d2))
     (combat (rest d1) (append (rest d2) (list (car d2) (car d1)))))
    (((car d1) . > . (car d2))
     (combat (append (rest d1) (list (car d1) (car d2))) (rest d2)))))

(define (score deck)
  (for/sum ((card (reverse deck))
            (i (in-naturals 1)))
    (* card i)))

;; Answer to part 1.
(combat (first decks) (second decks))

;; Part 2, on the other hand, is quite convoluted. This is why I didn't put
;; it together with part 1 into the same function, as it's already complicated
;; enough.
;;
;; We use a set (`seen`) to keep track of decks that have already appeared in the
;; game. Here I'm using a mutable set. Immutable sets can also be used (if
;; they are passed to `loop` each time), but in this case are slower by an
;; order of magnitude, so I went with a mutable set instead.
;;
;; Instead of returning just the score, we return a cons cell of the winning
;; player and their score, to be able to decide the winners of recursive games.
(define (recursive-combat first-deck second-deck)
  (define seen (mutable-set))
  (define (loop deck1 deck2)
    (cond
      ((null? deck1) (cons 2 (score deck2)))
      ((null? deck2) (cons 1 (score deck1)))
      ((set-member? seen (append (cons 1 deck1) (cons 2 deck2))) (cons 1 (score deck1)))
      ((and ((car deck1) . <= . (length (rest deck1)))
            ((car deck2) . <= . (length (rest deck2))))
       (set-add! seen (append (cons 1 deck1) (cons 2 deck2)))
       (if (= 1 (car (recursive-combat (take (rest deck1) (car deck1))
                                       (take (rest deck2) (car deck2)))))
           (loop (append (rest deck1) (list (car deck1) (car deck2))) (rest deck2))
           (loop (rest deck1) (append (rest deck2) (list (car deck2) (car deck1))))))
      (((car deck1) . < . (car deck2))
       (set-add! seen (append (cons 1 deck1) (cons 2 deck2)))
       (loop (rest deck1) (append (rest deck2) (list (car deck2) (car deck1)))))
      (((car deck1) . > . (car deck2))
       (set-add! seen (append (cons 1 deck1) (cons 2 deck2)))
       (loop (append (rest deck1) (list (car deck1) (car deck2))) (rest deck2)))))
  (loop first-deck second-deck))

;; Since we return both the winner and their score as a pair, we take the `cdr`
;; of the result to get the answer to part 2.
(cdr (recursive-combat (first decks) (second decks)))
