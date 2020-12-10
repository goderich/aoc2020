#lang racket

;; I like using threading macros to avoid deeply nested sexps, but this
;; is a matter of personal taste. No imports are required for this day.
(require threading)

;; Part 1
;;
;; We first build a sorted lists of all adaptors, and include the socket
;; (which is 0), and the maximum joltage (which is the highest joltage
;; of all adaptors plus 3).
(define jolts
  (let* ((adaptors (map string->number (file->lines "inputs/day10.txt")))
         (max-joltage (+ 3 (apply max adaptors))))
    (~>> adaptors
         (cons 0)
         (cons max-joltage)
         (sort _ <))))

;; A simple way to calculate intermediate distances is by using two lists,
;; one with the first element clipped and the other with the final element
;; clipped. We can then map subtraction to the two lists to get the distances
;; between adjacent elements of the initial list.
(define distances
  (map -
       (rest jolts)
       (drop-right jolts 1)))

;; The answer to part 1 is multiplication of counts.
;; I use currying here to simplify what would have otherwise been a lambda.
(* (count (curry = 1) distances)
   (count (curry = 3) distances))

;; Part 2
;;
;; There are many possible solutions to this part of the puzzle. My
;; solution relies on combinatorics rather than building up lists or
;; hash maps and mutating them. It is also incredibly fast. However,
;; it relies on a very important assumption about the input:
;;
;; - There are only distances of 1 and 3 between adjacent adaptors.
;;
;; As far as I know, this assumption holds for everyone's input, and
;; not just for mine.
;;
;; This lets us view the distances list as a series of streaks of
;; 1s and 3s. We are looking for possible arrangements/combinations/paths
;; of the sorted list of adaptors. Any distance of 3, and by extension
;; any streak of 3s, has only one possible path through it. Which means
;; that we need to calculate the paths in each streak of 1s and then
;; multiply them together to get the final answer.
;;
;; Moreover, we don't even need the streaks themselves, but just their
;; lengths. The function below calculates the lengths of streaks of
;; consecutive 1s in the distances list.
;; (NB: we don't even need to reverse the list in the end, because we'll
;; be calculating its product later, but I still chose to do it for
;; propriety's sake.)
(define (consecutive-ones lst)
  (for/fold ((num-ones 0)
             (acc '())
             #:result (reverse acc))
            ((x lst))
    (cond
      ((= 1 x) (values (add1 num-ones) acc))
      ((= 0 num-ones) (values num-ones acc))
      (else (values 0 (cons num-ones acc))))))

;; To find out the number of paths in a streak of 1-jolt distances, we
;; use the Tribonacci sequence, where the next number in the sequence is
;; the sum of the previous three numbers: 0, 1, 1, 2, 4, 7, 13, 24, etc.
;; However there are only sequences with length up to 4 in the puzzle, so
;; we can hardcode it here to save ourselves the trouble.
(define (numpaths len)
  (match len
    (1 1)
    (2 2)
    (3 4)
    (4 7)))

;; The product of the number of paths in each separate streak is the total
;; number of paths, and thus the answer.
(~>> distances
     (consecutive-ones)
     (map numpaths)
     (apply *))
