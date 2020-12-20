#lang racket

;; I considered using Megaparsack for this problem, but decided that it
;; would be unnecessary for several reasons:
;; 1. it would greatly slow down the process with huge and complicated
;; parsers,
;; 2. it would require a lot of try/p to make sure that all paths were
;; tried correctly, slowing it down even more,
;; 3. the puzzle input seems to be made with regex in mind (e.g. the use
;; of | as a logical OR separator).
;; For these reasons I stuck to regexps, but that did require a 'cheat'
;; of sorts in part 2, as you'll see below.
(require threading)

;; The input consists of two parts: the rules and the messages to be checked
;; against said rules. We parse them both into lists of lines.
(match-define (list rule-strs messages)
  (~>> "inputs/day19.txt"
       (file->string)
       (string-split _ "\n\n")
       (map (λ (lines) (string-split lines "\n")))))

;; I decided not to parse the rules at this stage, just the rule numbers,
;; storing the right half as a string in a hash map.
(define (parse-rule line)
  (match-define (list rulenum more) (string-split line ":"))
  (values (string->number rulenum) more))

(define rules
  (for/hasheq ((line rule-strs))
    (parse-rule line)))

;; Part 1 and part 2 are similar enough that we can use the same function
;; to build a regex for both of them, but it does require passing additional
;; information. Rather than function variables passed to the function
;; explicitly, I decided to use parameters instead.
(define part2?
  (make-parameter #f))

(define repeats
  (make-parameter 1))

;; This is the function that constructs a regex recursively from the rules.
;; Anything that's not "a" or "b" incurs a recursive call. The | chars are
;; left as is, and the rest are put together into a parenthesized group,
;; joined with no spacing.
(define (build-regex n)
  (define v (hash-ref rules n))
  (cond
    ((string-contains? v "\"") (last (string-split v "\"")))
    ;; The following two rules are used for part 2. Rule 8 is quite simple,
    ;; as it's just one or more occurrences of rule 42.
    ((and (part2?) (= n 8))
     (~a "(" (build-regex 42) ")+"))
    ;; Rule 11 on the other hand was more difficult with regex, and required
    ;; the use of an additional parameter and the aforementioned 'cheat'.
    ;; Because of how it's formulated (11: 42 31 | 42 11 31), it can be viewed
    ;; as (42)+(31)+, but the amount of both rules has to be identical.
    ;; I do not know of a way to do this with regex alone, so I opted for a
    ;; different approach: I used a parameter that was passed to both parts
    ;; in {curly brackets} to ensure the same amount of repetitions, and then
    ;; called the function several times with different values of `repeats`.
    ((and (part2?) (= n 11))
     (~a "(" (build-regex 42) "){" (repeats) "}(" (build-regex 31) "){" (repeats) "}"))
    (else
     (define (read-value s)
       (match s
         ("|" s)
         (_ (build-regex (string->number s)))))
     (~>> v
          (string-split)
          (map read-value)
          (string-join _ "")
          (~a "(" _ ")")))))

;; This function counts all exact matches of the 0 rule. We specifically
;; need to use `pregexp` and not `regexp` here because of {n,m} patters
;; for rule 11, which are only available in the former.
(define (valid-msg-count)
  (count (curry regexp-match-exact? (pregexp (build-regex 0))) messages))

;; Answer to part 1
(valid-msg-count)

;; For part 2, we turn on the `part2?` switch and parameterize `repeats`
;; from 1 to 5, adding up the resulting values. Inelegant, but hey, it works.
(for/sum ((k (in-range 1 5)))
  (parameterize ((repeats k)
                 (part2? #t))
    (valid-msg-count)))
