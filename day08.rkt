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
;; `acc` is the running tally of the accumulator value.
;; `ran` is the set of lines that we already ran before.
;; `linenum` is the current line number (duh).
;; `instr` is the hash map of instructions
;; (we need it as a parameter for part 2).
;; If the instructions terminate successfully, it returns
;; a Success value. If they go into a loop, it returns a
;; Failure value. Both also carry the value of `acc` at
;; the time of termination (Success and Failure come from
;; data/either).
;; If none of the above apply, it goes into a recursive
;; call based on the current instruction.
(define (run-instructions-rec acc ran linenum instr)
  (cond
    ((linenum . >= . (hash-count instr)) (success acc))
    ((set-member? ran linenum) (failure acc))
    (else
     (define ops (hash-ref instr linenum))
     (define op (car ops))
     (define val (cdr ops))
     (match op
       ("nop" (run-instructions-rec acc (set-add ran linenum) (add1 linenum) instr))
       ("acc" (run-instructions-rec (+ acc val) (set-add ran linenum) (add1 linenum) instr))
       ("jmp" (run-instructions-rec acc (set-add ran linenum) (+ val linenum) instr))))))

;; Since we always use the same values of `acc`, `ran`,
;; and `linenum` for the initial call of run-instructions-rec,
;; we can use a wrapper function to make it simpler and
;; more readable.
(define (run-instructions lst)
  (run-instructions-rec 0 '() 0 lst))

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
  (for/list (((key value) instructions))
    (define new-op (match (car value)
                      ("nop" "jmp")
                      ("jmp" "nop")
                      ("acc" "acc")))
    (hash-set instructions key (cons new-op (cdr value)))))

;; Then it is just a matter of iterating over the list of
;; instruction sets until we get one that terminates in a
;; Success value.
(for/first ((instruction-lst altered-instructions)
            #:when (success? (run-instructions instruction-lst)))
  (from-success #f (run-instructions instruction-lst)))
