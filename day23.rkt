#lang racket

;; Today I'm using `mischief/for` apart from my usual `threading` library.
;; I use the former for its `for/hash!` functionality, to create mutable
;; hash maps with looping constructs.
(require threading
         mischief/for)

;; We begin by parsing the input into a list of ints.
(define input
  (~>> (file->string "inputs/day23.txt")
       (string-trim)
       (string->list)
       (map (curry make-string 1))
       (map string->number)))

;; But we will in fact be using mutable hash maps as impromptu linked lists
;; (see explanation above the `game` function). For this we write this helper
;; function that creates a "linked dict" out of a list.
(define (make-linked-dict lst)
  ;; Vectors have faster element access, this makes a difference for part 2.
  (define vec (list->vector lst))
  ;; We use `for/hasheq!` from the `mischief` library.
  (for/hasheq! ((i (vector-length vec)))
    (values (vector-ref vec i)
            (if (= i (sub1 (vector-length vec)))
                (vector-ref vec 0)
                (vector-ref vec (add1 i))))))

;; The dictionary we'll be using in part 1.
(define part1-dict
  (make-linked-dict input))

;; Parts 1 and 2 have identical rules, part 2 just has a much larger input
;; and many more moves. That makes it impractical (if not outright impossible)
;; to use lists in part 2.
;;
;; However, notice that with each move, the only change is the relocation of
;; three cups. The rest of the cups do not move. We do need to quickly find
;; the locations of multiple cups during each move, so using a hash map makes
;; perfect sense here. We use cup labels as keys, and the next cup as the dict
;; value, with the last cup pointing to the first cup in a closed loop, very
;; much like a linked list, but with constant time element access.
;;
;; Just like yesterday, I opted for mutable hash maps after trying both mutable
;; and immutable variants. Mutable ones take about 5 seconds, and immutable
;; ones take 50+ seconds for part 2. With such large datasets and move numbers,
;; the difference is significant, and thus I'm using mutable hash maps.
;;
;; Because part 1 and part 2 have the same rules, we can use the same function
;; for both of them. We provide it the mutable hash map and the number of
;; moves in the game, and it returns the dict after completing the game.
(define (game dict num-moves)
  (define (loop current-cup move)
    (cond
      ((= move num-moves) dict)
      (else
       ;; a, b, and c are the three cups after the current cup
       (define a (hash-ref dict current-cup))
       (define b (hash-ref dict a))
       (define c (hash-ref dict b))
       (define (find-destination-cup label)
         (cond
           ((zero? label) (find-destination-cup (hash-count dict)))
           ((member label (list a b c)) (find-destination-cup (sub1 label)))
           (else label)))
       (define destination-cup (find-destination-cup (sub1 current-cup)))
       ;; We're moving the destination cup first, so we need a temporary
       ;; variable that points to its previous location.
       (define cup-old-tail (hash-ref dict destination-cup))
       (hash-set! dict destination-cup a)
       (hash-set! dict current-cup (hash-ref dict c))
       (hash-set! dict c cup-old-tail)
       (loop (hash-ref dict current-cup) (add1 move)))))
  (loop 3 0))

;; The answer to part 1 is a string containing all the digits following
;; cup number 1 in the hash map.
(define (part1-answer dict)
  (define (loop curr acc)
    (if (= curr 1)
        (apply string-append (map number->string (reverse acc)))
        (loop (hash-ref dict curr) (cons curr acc))))
  (loop (hash-ref dict 1) '()))

;; Output the answer to part 1.
(part1-answer (game part1-dict 100))

;; Part 2
;;
;; Having written the `game` function, we only need to create the huge list
;; for its input and perform the necessary calculations.
(define part2-dict
  (make-linked-dict (append input (range 10 1000001))))

;; The answer to part 2 is the product of the two cup labels following 1.
(define (part2-answer dict)
  (* (hash-ref dict 1)
     (hash-ref dict (hash-ref dict 1))))

;; Output the answer to part 2.
(part2-answer (game part2-dict 10000000))
