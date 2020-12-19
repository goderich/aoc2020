#lang racket

(require threading)

(match-define (list rule-strs messages)
  (~>> "inputs/day19.txt"
       (file->string)
       (string-split _ "\n\n")
       (map (λ (lines) (string-split lines "\n")))))

(define (parse-rule line)
  (match-define (list rulenum more) (string-split line ":"))
  (values (string->number rulenum) more))

(define rules
  (for/hasheq ((line rule-strs))
    (parse-rule line)))

(define part2?
  (make-parameter #f))

(define repeats
  (make-parameter 1))

(define (build-regex n)
  (define v (hash-ref rules n))
  (cond
    ((string-contains? v "\"") (last (string-split v "\"")))
    ((and (part2?) (= n 8))
     (~a "(" (build-regex 42) ")+"))
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

(define (valid-msg-count)
  (count (curry regexp-match-exact? (pregexp (build-regex 0))) messages))

(valid-msg-count)

(for/sum ((k (in-range 1 5)))
  (parameterize ((repeats k)
                 (part2? #t))
    (valid-msg-count)))
