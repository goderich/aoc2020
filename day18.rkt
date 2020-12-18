#lang racket

(require megaparsack
         megaparsack/text
         data/monad
         data/applicative
         data/either)

(define add/p
  (do (char/p #\+)
      (pure +)))

(define multiply/p
  (do (char/p #\*)
      (pure *)))

(define operator/p
  (or/p add/p multiply/p))

(define parenthesized/p
  (do (char/p #\()
      (inside <- (many/p (do (many/p space/p #:max 1)
                             (or/p integer/p operator/p parenthesized/p))))
      (char/p #\))
      (pure inside)))

(define expression/p
  (many/p (do (many/p space/p #:max 1)
              (or/p integer/p operator/p parenthesized/p))))

(define (eval1 lst)
  (for/fold ((acc 0)
             (op +)
             #:result acc)
            ((c lst))
    (cond
      ((list? c) (values (op acc (eval1 c)) #f))
      ((number? c) (values (op acc c) #f))
      ((procedure? c) (values acc c)))))

(for/sum ((line (file->lines "inputs/day18.txt")))
  (eval1 (from-success #f (parse-string expression/p line))))

(define (no-parens-loop input)
  (for/fold ((lst input)
             #:result (apply * (filter number? lst)))
            ((_ (indexes-of input +)))
    (define i (index-of lst +))
    (define prev (list-ref lst (sub1 i)))
    (define next (list-ref lst (add1 i)))
    (append (take lst (sub1 i))
            (list (+ prev next))
            (drop lst (+ 2 i)))))

(define (eval2 xs)
  (define i (index-where xs list?))
  (cond
    ((not i) (no-parens-loop xs))
    (else
     (eval2 (list-set xs i (eval2 (list-ref xs i)))))))


(for/sum ((line (file->lines "inputs/day18.txt")))
  (eval2 (from-success #f (parse-string expression/p line))))
