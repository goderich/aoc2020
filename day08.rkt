#lang racket

;; For today's solution we will pull the data/either
;; library (also by Alexis King) in order to classify
;; the two kinds of outcomes we can get.
(require data/either)

;; First, we read in the instructions. The operation
;; and number on each line are stored in a cons cell.
(define (parse-instruction instr)
  (match-define (list op num) (string-split instr))
  (cons op (string->number num)))

;; The instructions themselves are read into a hash map,
;; with line numbers as keys, for fast access.
(define instructions
  (for/hasheq ((linenum (in-naturals))
               (line (file->lines "inputs/day08.txt")))
    (values linenum (parse-instruction line))))

;; This is where the magic happens. This function is
;; generalized for both parts of the puzzle.
;; The embedded `loop` function does all the work,
;; while the `run-instructions` function simply presents
;; a clean interface.
;; `acc` is the running tally of the accumulator value.
;; `ran` is the set of lines that we already ran before.
;; `linenum` is the current line number (duh).
;; If the instructions terminate successfully, it returns
;; a Success value. If they go into a loop, it returns a
;; Failure value. Both also carry the value of `acc` at
;; the time of termination (Success and Failure come from
;; data/either).
;; If none of the above apply, it goes into a recursive
;; call based on the current instruction.
;; Since we always use the same values of `acc`, `ran`,
;; and `linenum` for the initial call of the loop,
;; we can embed it in a wrapper function to make it simpler
;; to use and its funcalls more readable.
;; Moreover, `loop` is a closure, so we don't need to
;; pass the `lst` variable to it directly, but `loop` can still
;; access it.
(define (run-instructions lst)
  (define list-len (hash-count lst))
  (define ran (mutable-seteq))
  (define (loop acc linenum)
    (cond
      ((linenum . >= . list-len) (success acc))
      ((set-member? ran linenum) (failure acc))
      (else
       (match-define (cons op val) (hash-ref lst linenum))
       (set-add! ran linenum)
       (match op
         ("nop" (loop acc (add1 linenum)))
         ("acc" (loop (+ acc val) (add1 linenum)))
         ("jmp" (loop acc (+ val linenum)))))))
  (loop 0 0))

;; Part 1
;;
;; The answer is the value returned when the instructions
;; go into a loop. Since run-instructions-rec returns a
;; Failure in that case, we extract the value from it
;; (we can also just leave it like that, it's still
;; readable in the terminal).
(from-failure #f (run-instructions instructions))

;; Part 2
;;
;; Here we first create a list of all possible instruction
;; sets where exactly one "jmp" is changed to "nop" or a "nop"
;; to "jmp".
(define altered-instructions
  (for/list (((key value) instructions)
             #:when (member (car value) '("nop" "jmp")))
    (define new-op (match (car value)
                      ("nop" "jmp")
                      ("jmp" "nop")))
    (hash-set instructions key (cons new-op (cdr value)))))

;; Then it is just a matter of iterating over the list of
;; instruction sets until we get one that terminates in a
;; Success value.
(for/first ((instruction-lst altered-instructions)
            #:when (success? (run-instructions instruction-lst)))
  (from-success #f (run-instructions instruction-lst)))
